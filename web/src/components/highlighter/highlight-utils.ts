// 高亮配置和映射
export interface HighlightSpan {
  start: number
  end: number
  cls: string
}

export interface RankedSpan extends HighlightSpan {
  p: number // priority
  len: number // length
}

/**
 * 从 Tree-sitter 捕获名称映射到 CSS 类名
 */
export function classFromCapture(name: string): string {
  if (name.includes('keyword'))     return 'keyword'
  if (name.includes('string'))      return 'string'
  if (name.includes('number'))      return 'number'
  if (name.includes('boolean'))     return 'boolean'
  if (name.includes('comment'))     return 'comment'
  if (name.includes('function'))    return 'function'
  if (name.includes('type'))        return 'type'
  if (name.includes('operator'))    return 'operator'
  if (name.includes('punctuation')) return 'punctuation'
  if (name.includes('attribute'))   return 'deprecated-anno' // 用 @attribute 呈现 #deprecated 等标注
  if (name.includes('constant'))    return 'constant'
  if (name.includes('variable'))    return 'identifier'
  return 'default'
}

/**
 * 获取 CSS 类的优先级（用于冲突解决）
 */
export function getPriority(cls: string): number {
  const priorityTable: Record<string, number> = {
    error: 100,
    keyword: 90,
    function: 80,
    type: 70,
    string: 60, 
    number: 60, 
    boolean: 60, 
    constant: 60,
    'deprecated-anno': 55,
    comment: 50,
    operator: 40,
    identifier: 20,
    punctuation: 10,
    default: 0
  }
  return priorityTable[cls] ?? 0
}

/**
 * 处理重叠片段，使用优先级解决冲突
 */
export function resolveOverlaps(spans: HighlightSpan[]): HighlightSpan[] {
  // 按优先级→长度→起点排序
  const ranked: RankedSpan[] = spans.map(s => ({
    ...s,
    p: getPriority(s.cls),
    len: s.end - s.start
  })).sort((a, b) => b.p - a.p || b.len - a.len || a.start - b.start)

  // 贪心去重
  const chosen: HighlightSpan[] = []
  for (const s of ranked) {
    const overlap = chosen.some(t => !(s.end <= t.start || s.start >= t.end))
    if (!overlap) {
      chosen.push({ start: s.start, end: s.end, cls: s.cls })
    }
  }

  return chosen.sort((a, b) => a.start - b.start)
}

/**
 * 合并相邻的同类片段（减少 DOM 节点）
 */
export function mergeAdjacentSpans(spans: HighlightSpan[]): HighlightSpan[] {
  if (spans.length === 0) return []

  const merged: HighlightSpan[] = []
  for (const span of spans) {
    const last = merged[merged.length - 1]
    if (last && last.end === span.start && last.cls === span.cls) {
      // 合并相邻同类
      last.end = span.end
    } else {
      merged.push({ ...span })
    }
  }
  return merged
}

/**
 * 将文本中的 HTML 特殊字符转义
 */
export function escapeHtml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')
}

/**
 * 将高亮片段应用到源码，生成 HTML
 */
export function applyHighlighting(source: string, spans: HighlightSpan[]): string {
  if (spans.length === 0) return escapeHtml(source)

  const resolved = resolveOverlaps(spans)
  const merged = mergeAdjacentSpans(resolved)

  let result = ''
  let pos = 0

  for (const span of merged) {
    // 添加前面的无高亮文本
    if (pos < span.start) {
      result += escapeHtml(source.slice(pos, span.start))
    }

    // 添加高亮的文本
    const content = escapeHtml(source.slice(span.start, span.end))
    result += `<span class="${span.cls}">${content}</span>`

    pos = span.end
  }

  // 添加剩余的无高亮文本
  if (pos < source.length) {
    result += escapeHtml(source.slice(pos))
  }

  return result
}