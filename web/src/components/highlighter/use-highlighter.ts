import { ref, readonly } from 'vue'
import type { Ref } from 'vue'

// 解析器状态接口
export interface ParserState {
  parser: any | null
  language: any | null
  query: any | null
}

// 高亮结果接口
export interface HighlightResult {
  success: boolean
  html: string
  error?: string
  stats?: {
    parseTime: number
    queryTime: number
    renderTime: number
  }
}

/**
 * Tree-sitter 解析器初始化 Hook
 */
export function useTreeSitterParser() {
  const parserState: Ref<ParserState> = ref({
    parser: null,
    language: null,
    query: null
  })

  const loading = ref(true)
  const error = ref<string | null>(null)

  const initializeParser = async (highlightsSource: string) => {
    loading.value = true
    error.value = null
    
    try {
      // 动态导入避免在编译时出错
      const { getMoonbitWithQuery } = await import('../ts-runtime')
      const result = await getMoonbitWithQuery(highlightsSource)
      
      parserState.value = {
        parser: result.parser,
        language: result.language,
        query: result.query
      }
      
      loading.value = false
    } catch (err) {
      const errorMsg = err instanceof Error ? err.message : String(err)
      error.value = `Parser initialization failed: ${errorMsg}`
      loading.value = false
      console.error('Failed to initialize parser:', err)
    }
  }

  return {
    parserState: readonly(parserState),
    loading: readonly(loading),
    error: readonly(error),
    initializeParser
  }
}

/**
 * 代码高亮处理 Hook
 */
export function useCodeHighlighter(parserState: Ref<ParserState>) {
  const highlightCode = (source: string): HighlightResult => {
    const { parser, language, query } = parserState.value

    if (!parser || !language) {
      return {
        success: false,
        html: '',
        error: 'Parser not initialized'
      }
    }

    if (!source.trim()) {
      return {
        success: true,
        html: ''
      }
    }

    try {
      const startTime = performance.now()
      
      // 1. 解析源码
      const tree = parser.parse(source)
      const parseTime = performance.now() - startTime

      // 2. 如果没有查询，返回纯文本
      if (!query) {
        // Import dynamically to avoid build issues
        const highlightUtils = await import('./highlight-utils')
        return {
          success: true,
          html: highlightUtils.escapeHtml(source),
          stats: {
            parseTime,
            queryTime: 0,
            renderTime: 0
          }
        }
      }

      // 3. 执行查询
      const queryStart = performance.now()
      const captures = query.captures(tree.rootNode)
      const queryTime = performance.now() - queryStart

      // 4. 生成高亮
      const renderStart = performance.now()
      const highlightUtils = await import('./highlight-utils')
      
      const spans = captures
        .map(({ node, name }: any) => ({
          start: node.startIndex,
          end: node.endIndex,
          cls: highlightUtils.classFromCapture(name)
        }))
        .filter((s: any) => s.end > s.start)

      const html = highlightUtils.applyHighlighting(source, spans)
      const renderTime = performance.now() - renderStart

      return {
        success: true,
        html,
        stats: {
          parseTime,
          queryTime,
          renderTime
        }
      }
    } catch (err) {
      const errorMsg = err instanceof Error ? err.message : String(err)
      console.error('Highlighting failed:', err)
      
      return {
        success: false,
        html: '',
        error: errorMsg
      }
    }
  }

  return {
    highlightCode
  }
}