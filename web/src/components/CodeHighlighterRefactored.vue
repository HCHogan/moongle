<template>
  <div class="moonbit-highlighter">
    <pre v-if="loading" class="loading">Loading syntax highlighting...</pre>
    <pre v-else-if="error" class="error">{{ error }}</pre>
    <pre v-else class="code-container">
      <code v-html="highlightedCode"></code>
      <div v-if="showStats && stats" class="highlight-stats">
        Parse: {{ stats.parseTime.toFixed(1) }}ms | 
        Query: {{ stats.queryTime.toFixed(1) }}ms | 
        Render: {{ stats.renderTime.toFixed(1) }}ms
      </div>
    </pre>
  </div>
</template>

<script setup lang="ts">
import { ref, watch, onMounted, computed } from 'vue'
import { useTreeSitterParser, useCodeHighlighter } from './highlighter/use-highlighter'
import highlightsSource from '@/assets/moonbit-highlight.scm?raw'

// Props
interface Props {
  code: string
  showStats?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  code: '',
  showStats: false
})

// State
const highlightedCode = ref('')
const stats = ref<any>(null)

// Composables
const { parserState, loading, error, initializeParser } = useTreeSitterParser()
const { highlightCode } = useCodeHighlighter(parserState)

// Computed
const hasValidCode = computed(() => props.code?.trim().length > 0)

// Methods
const performHighlighting = () => {
  if (!hasValidCode.value) {
    highlightedCode.value = ''
    stats.value = null
    return
  }

  if (loading.value || error.value || !parserState.value.parser) {
    return
  }

  const result = highlightCode(props.code)
  
  if (result.success) {
    highlightedCode.value = result.html
    stats.value = result.stats
  } else {
    console.error('Highlighting failed:', result.error)
    // Fallback to escaped plain text
    const highlightUtils = await import('./highlighter/highlight-utils')
    highlightedCode.value = highlightUtils.escapeHtml(props.code)
  }
}

// Lifecycle
onMounted(async () => {
  await initializeParser(highlightsSource)
  performHighlighting()
})

// Watchers
watch(() => props.code, performHighlighting)
watch([loading, error], performHighlighting)
</script>

<style scoped>
.moonbit-highlighter {
  position: relative;
}

.code-container {
  margin: 0;
  padding: 1rem;
  background-color: #f8f9fa;
  border: 1px solid #e9ecef;
  border-radius: 4px;
  overflow-x: auto;
  font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
  font-size: 0.9rem;
  line-height: 1.4;
}

.loading {
  color: #6c757d;
  font-style: italic;
  margin: 0;
  padding: 1rem;
}

.error {
  color: #dc3545;
  background-color: #f8d7da;
  border: 1px solid #f5c6cb;
  border-radius: 4px;
  margin: 0;
  padding: 1rem;
}

.highlight-stats {
  position: absolute;
  top: 0.5rem;
  right: 0.5rem;
  font-size: 0.75rem;
  color: #6c757d;
  background: rgba(255, 255, 255, 0.8);
  padding: 0.25rem 0.5rem;
  border-radius: 3px;
  font-family: monospace;
}

/* Syntax highlighting styles */
:deep(.keyword) { color: #0000ff; font-weight: bold; }
:deep(.string) { color: #008000; }
:deep(.number) { color: #0000ff; }
:deep(.boolean) { color: #0000ff; }
:deep(.comment) { color: #808080; font-style: italic; }
:deep(.function) { color: #795e26; }
:deep(.type) { color: #267f99; }
:deep(.operator) { color: #000000; }
:deep(.punctuation) { color: #000000; }
:deep(.deprecated-anno) { color: #ff0000; text-decoration: line-through; }
:deep(.constant) { color: #0070c1; }
:deep(.identifier) { color: #001080; }
:deep(.error) { color: #ff0000; background-color: #ffe6e6; }
</style>