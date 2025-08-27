<template>
  <div class="moonbit-highlighter">
    <pre v-if="loading" class="loading">Loading syntax highlighting...</pre>
    <pre v-else-if="error" class="error">{{ error }}</pre>
    <pre v-else class="code-container"><code v-html="highlightedCode"></code></pre>
  </div>
</template>

<script>
// Tree-sitter + .scm（若使用 scm 模式时才会用到）
import { getMoonbitWithQuery } from './ts-runtime'
import highlightsSource from '@/assets/moonbit-highlight.scm?raw'

export default {
  name: 'CodeHighlighter',
  props: {
    code: { type: String, required: true, default: '' },
    /**
     * 'simple' 仅用正则/函数高亮（稳）
     * 'scm'    强制用 .scm（tree-sitter）高亮
     * 'auto'   优先 .scm，失败自动降级 simple
     */
    mode: { type: String, default: 'simple' }
  },
  data () {
    return {
      parser: null,
      language: null,
      query: null,
      highlightedCode: '',
      loading: true,
      error: null
    }
  },
  async mounted () {
    await this.initializeParser()
  },
  watch: {
    code () { this.highlightCode() },
    mode () { this.initializeParser() }
  },
  methods: {
    async initializeParser () {
      this.loading = true
      this.error = null
      try {
        if (this.mode === 'simple') {
          // 直接走简单高亮，不加载 parser/query
          this.parser = null
          this.language = null
          this.query = null
          this.loading = false
          this.highlightCode()
          return
        }

        const { parser, language, query } = await getMoonbitWithQuery(highlightsSource)
        this.parser = parser
        this.language = language
        this.query = (this.mode === 'scm' || this.mode === 'auto') ? query : null

        this.loading = false
        this.highlightCode()
      } catch (err) {
        console.error('Failed to initialize parser:', err)
        if (this.mode === 'auto') {
          // 自动降级到 simple
          this.parser = null
          this.language = null
          this.query = null
          this.error = null
          this.loading = false
          this.highlightCode()
        } else {
          this.error = `Initialization failed: ${err?.message || err}`
          this.loading = false
        }
      }
    },

    /** —— 简单高亮：线性扫描、不中断连字 —— */
    simpleHighlight (src) {
      const patterns = [
        { cls: 'comment',  re: /\/\*[\s\S]*?\*\/|\/\/[^\n]*/y },
        { cls: 'string',   re: /"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'/y },
        { cls: 'number',   re: /\b0x[0-9A-Fa-f]+|\b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?\b/y },
        { cls: 'keyword',  re: /\b(?:fn|let|mut|var|if|else|match|while|for|return|break|continue|struct|enum|type|impl|Self|self|pub|use|mod|package|where|as|in|const|static|trait|extern|async|await|try|catch|throw|new)\b/y },
        { cls: 'boolean',  re: /\b(?:true|false)\b/y },
        { cls: 'function', re: /\b[A-Za-z_][A-Za-z0-9_]*(?=\s*\()/y },
        { cls: 'type',     re: /\b[A-Z][A-Za-z0-9_]*\b/y },
        { cls: 'operator', re: /[+\-*/%=!<>|&^~?:]+/y },
        { cls: 'punctuation', re: /[()[\]{}.,;:@]/y },
        { cls: 'identifier',  re: /\b[A-Za-z_][A-Za-z0-9_]*\b/y },
      ]

      let i = 0, out = ''
      while (i < src.length) {
        let hit = false
        for (const p of patterns) {
          p.re.lastIndex = i
          const m = p.re.exec(src)
          if (m && m.index === i) {
            out += `<span class="token ${p.cls}">${this.escapeHtml(m[0])}</span>`
            i += m[0].length
            hit = true
            break
          }
        }
        if (!hit) { out += this.escapeHtml(src[i]); i++ }
      }
      return out
    },

    /** —— 工具：把范围扩到完整单词（给 scm 分支用） —— */
    expandToWord (src, start, end) {
      let s = start, e = end
      const isWord = c => /[A-Za-z0-9_]/.test(c)
      while (s > 0 && isWord(src[s - 1])) s--
      while (e < src.length && isWord(src[e])) e++
      return { start: s, end: e }
    },

    /** —— 主流程：根据 mode 选择 simple 或 scm —— */
    highlightCode () {
      if (this.loading) return
      const src = this.code ?? ''
      if (!src) { this.highlightedCode = ''; return }

      // ✅ 简单高亮（默认/降级）
      if (this.mode === 'simple' || !this.query) {
        this.highlightedCode = this.simpleHighlight(src)
        return
      }

      // —— .scm 分支（tree-sitter）——
      try {
        const tree = this.parser.parse(src)
        let caps = this.query.captures(tree.rootNode) || []
        caps = caps.filter(c => c.name !== 'error' && c.node?.type !== 'ERROR')

        // 1) 把超短（≤2）的 capture 先吸附到完整单词
        let spans = caps.map(({ node, name }) => {
          let start = node.startIndex
          let end   = node.endIndex
          const cls = this.classFromCapture(name)

          if ((end - start) <= 2) {
            const ex = this.expandToWord(src, start, end)
            if ((ex.end - ex.start) > (end - start)) { start = ex.start; end = ex.end }
          }

          // 记录该片段所在“单词跨度”用于排序
          const w = this.expandToWord(src, start, end)
          const wlen = w.end - w.start

          return { start, end, cls, wlen }
        }).filter(s => s.end > s.start)

        // 2) 剪掉被更长“词跨度”完全覆盖的超短片段（避免 Self 只剩 S）
        spans = spans.filter((s, i, arr) => {
          const slen = s.end - s.start
          if (slen > 2) return true
          return !arr.some((t, j) =>
            j !== i &&
            t.start <= s.start && t.end >= s.end &&
            (t.end - t.start) > slen &&
            t.wlen >= s.wlen
          )
        })

        // 3) 排序：优先“所属单词更长”→ 片段更长 → 语法优先级 → 起点
        const ranked = spans.map(s => ({
          ...s,
          p: this.priorityOf(s.cls),
          len: s.end - s.start
        })).sort((a, b) =>
          b.wlen - a.wlen ||
          b.len  - a.len  ||
          b.p    - a.p    ||
          a.start - b.start
        )

        // 4) 贪心选片，避免重叠
        const chosen = []
        for (const s of ranked) {
          let overlap = false
          for (const t of chosen) {
            if (!(s.end <= t.start || s.start >= t.end)) { overlap = true; break }
          }
          if (!overlap) chosen.push({ start: s.start, end: s.end, cls: s.cls })
        }
        chosen.sort((a, b) => a.start - b.start)

        // 5) 合并相邻同类
        const merged = []
        for (const s of chosen) {
          const last = merged[merged.length - 1]
          if (last && last.cls === s.cls && last.end === s.start) last.end = s.end
          else merged.push({ ...s })
        }

        // 6) 渲染
        let html = '', i = 0
        for (const s of merged) {
          if (i < s.start) html += this.escapeHtml(src.slice(i, s.start))
          html += `<span class="token ${s.cls}">${this.escapeHtml(src.slice(s.start, s.end))}</span>`
          i = s.end
        }
        if (i < src.length) html += this.escapeHtml(src.slice(i))
        this.highlightedCode = html && html.trim() ? html : this.escapeHtml(src)
      } catch (err) {
        console.error('Code highlighting failed:', err)
        // 出错则降级 simple
        this.highlightedCode = this.simpleHighlight(src)
      }
    },

    // capture 名 → CSS 类名（scm 分支时使用）
    classFromCapture (name) {
      if (!name) return 'default'
      if (name.includes('keyword'))     return 'keyword'
      if (name.includes('function'))    return 'function'
      if (name.includes('type'))        return 'type'
      if (name.includes('string'))      return 'string'
      if (name.includes('number'))      return 'number'
      if (name.includes('boolean'))     return 'boolean'
      if (name.includes('comment'))     return 'comment'
      if (name.includes('operator'))    return 'operator'
      if (name.includes('punctuation')) return 'punctuation'
      if (name.includes('attribute'))   return 'deprecated-anno'
      if (name.includes('constant'))    return 'constant'
      if (name.includes('variable'))    return 'identifier'
      return 'default'
    },

    priorityOf (cls) {
      const table = {
        error: 100,
        keyword: 90,
        function: 80,
        type: 70,
        string: 60, number: 60, boolean: 60, constant: 60,
        'deprecated-anno': 55,
        comment: 50,
        operator: 40,
        identifier: 20,
        punctuation: 10,
        default: 0
      }
      return table[cls] ?? 0
    },

    escapeHtml (text) {
      const map = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }
      return (text || '').replace(/[&<>"']/g, m => map[m])
    }
  },

  beforeUnmount () {
    try { this.parser?.delete?.() } catch {}
    try { this.language?.delete?.() } catch {}
  }
}
</script>

<style scoped>
.moonbit-highlighter{
  font-family: 'Consolas','Monaco','Courier New',monospace;
  color-scheme: light dark;
}

/* 代码块底/边/字色：走站点变量，暗色自动跟随 */
    /* 容器走变量 */
.code-container{
  background-color: var(--code-bg);
  border: 1px solid var(--code-border);
  color: var(--code-fg);
  border-radius: 8px;
  padding: 16px;
  margin: 0;
  overflow-x: auto;
  line-height: 1.45;
  font-size: 14px;
  -webkit-backdrop-filter: saturate(180%) blur(12px);
  backdrop-filter: saturate(180%) blur(12px);
}

/* 统一重置，防止斜体与半黑半白 */
.code-container :deep(.token),
.code-container :deep(code){
  font-style: normal !important;
  -webkit-text-fill-color: currentColor !important;
  font-variant-ligatures: none !important;
  font-feature-settings: "liga" 0, "clig" 0, "calt" 0 !important;
}
.code-container :deep(.token.comment),
.code-container :deep(.token.deprecated-anno){ font-style: italic !important; }

/* Token 颜色 → 统统引用变量（无需再写暗色选择器） */
.code-container :deep(.token.keyword){      color: var(--tok-kw)    !important; font-weight:600; }
.code-container :deep(.token.function){     color: var(--tok-fn)    !important; font-weight:600; }
.code-container :deep(.token.type){         color: var(--tok-type)  !important; font-weight:600; }
.code-container :deep(.token.string){       color: var(--tok-str)   !important; }
.code-container :deep(.token.number){       color: var(--tok-num)   !important; }
.code-container :deep(.token.boolean){      color: var(--tok-bool)  !important; }
.code-container :deep(.token.comment){      color: var(--tok-comm)  !important; }
.code-container :deep(.token.operator){     color: var(--tok-op)    !important; }
.code-container :deep(.token.constant){     color: var(--tok-const) !important; }
.code-container :deep(.token.identifier){   color: var(--tok-id)    !important; }
.code-container :deep(.token.punctuation),
.code-container :deep(.token.default){      color: var(--tok-punc)  !important; }

</style>

