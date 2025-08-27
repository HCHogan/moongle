<script setup lang="ts">
import { ref, onMounted } from 'vue'

interface Stats {
  decls: number
  modules: number
  lastIndexed: string
}

const stats = ref<Stats | null>(null)
const loading = ref(true)
const error = ref<string | null>(null)

async function fetchStats() {
  loading.value = true
  error.value = null
  try {
    const response = await fetch('/api/stats')

    if (!response.ok) {
      if (response.status === 500) {
        error.value = 'Internal Server Error (500), please try again later or contact an administrator.'
      } else {
        const errorText = await response.text()
        error.value = `The server returned an unexpected response: ${response.status} - ${errorText}`
      }
      return
    }

    const data = await response.json()

    if (data.status === 'ok' && data.dat) {
      const received = data.dat
      if (
        typeof received.decls === 'number' &&
        typeof received.modules === 'number' &&
        typeof received.lastIndexed === 'string'
      ) {
        stats.value = received
      } else {
        error.value = 'Received invalid statistics format from the server.'
      }
    } else {
      error.value = data.err?.message || 'Failed to fetch statistics.'
    }
  } catch (e: any) {
    if (e instanceof SyntaxError) {
      error.value = 'Could not parse the successful server response as JSON.'
    } else {
      error.value = `Request failed: ${e.message}`
    }
  } finally {
    loading.value = false
  }
}

function formatDateTime(isoString: string) {
  if (!isoString) return 'N/A'
  return new Date(isoString).toLocaleString()
}

onMounted(() => {
  fetchStats()
})
</script>

<template>
  <div class="page-container">
    <h1>Statistics</h1>

    <div v-if="loading" class="loading-indicator">
      <p>Loading...</p>
    </div>

    <div v-else-if="error" class="error-message">
      <p>Failed to load data: {{ error }}</p>
      <button @click="fetchStats">Retry</button>
    </div>

    <div v-else-if="stats" class="stats-grid">
      <div class="stat-item">
        <div class="stat-value">{{ stats.decls.toLocaleString() }}</div>
        <div class="stat-label">Declarations</div>
      </div>

      <div class="stat-item">
        <div class="stat-value">{{ stats.modules.toLocaleString() }}</div>
        <div class="stat-label">Modules</div>
      </div>

      <div class="stat-item stat-item-full">
        <div class="stat-label">Last Indexed Time</div>
        <div class="stat-value-small">{{ formatDateTime(stats.lastIndexed) }}</div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* -------- 基础排版 -------- */
.page-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  width: 100%;

  /* 轨道宽度更克制，避免卡片太宽显空 */
  max-width: 780px;
  margin: 0 auto;
  padding: 56px 24px 80px;

  box-sizing: border-box;
  text-align: center;

  /* 若外部未提供变量，则用兜底色值 */
  color: var(--ios-fg, #1c1c1e);
  font-family: -apple-system, BlinkMacSystemFont, "SF Pro Text", "SF Pro Display", "Helvetica Neue", Arial, sans-serif;
  letter-spacing: -0.01em;
}
h1 {
  margin: 0 0 28px;
  font-weight: 700;
  font-size: clamp(22px, 3.2vw, 28px);
}

/* -------- 加载/错误 -------- */
.loading-indicator p,
.error-message p {
  color: var(--ios-muted, #8e8e93);
  margin: 16px 0 0;
}
.error-message button {
  margin-top: 12px;
  padding: 8px 14px;
  border-radius: 10px;
  border: 0.5px solid rgba(0,0,0,0.08);
  background: rgba(255,255,255,0.72);
  color: var(--ios-fg, #1c1c1e);
  cursor: pointer;
  -webkit-backdrop-filter: saturate(180%) blur(14px);
  backdrop-filter: saturate(180%) blur(14px);
  transition: transform .2s ease, box-shadow .2s ease;
}
.error-message button:hover {
  transform: translateY(-1px);
  box-shadow: 0 6px 16px rgba(0,0,0,0.08);
}

/* -------- 自适应布局 -------- */
/* 默认单列；≥760px 两列；信息卡始终整行 */
.stats-grid {
  display: grid;
  grid-template-columns: 1fr;      /* 手机：单列 */
  gap: 24px;                       /* 拉开卡片距离 */
  width: 100%;
  align-items: stretch;            /* 等高 */
}
@media (min-width: 760px) {
  .stats-grid { grid-template-columns: repeat(2, 1fr); } /* 桌面：两列 */
}
.stat-item-full { grid-column: 1 / -1; }                 /* 信息卡整行 */

/* -------- 卡片视觉 -------- */
.stat-item {
  min-height: 132px;
  padding: 26px;
  border-radius: 14px;

  /* 毛玻璃 + 发丝边 + 轻阴影 */
  background: rgba(255,255,255,0.85);
  -webkit-backdrop-filter: saturate(180%) blur(18px);
  backdrop-filter: saturate(180%) blur(18px);
  border: 0.5px solid rgba(0,0,0,0.08);
  box-shadow: 0 8px 30px rgba(0,0,0,0.06), 0 2px 10px rgba(0,0,0,0.04);

  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
}
.stat-item-full {
  min-height: 84px;
  padding: 18px 22px;
  align-items: flex-start;
  text-align: left;
}

/* 暗黑模式兜底 */
html.dark .stat-item {
  background: rgba(28,28,30,0.72);
  border: 0.5px solid rgba(255,255,255,0.12);
  box-shadow: 0 12px 40px rgba(0,0,0,0.5), 0 2px 10px rgba(0,0,0,0.35);
}

/* -------- 字级与对齐 -------- */
.stat-value {
  font-weight: 700;
  font-size: clamp(28px, 2.6vw + 8px, 40px); /* 桌面最大 40px */
  line-height: 1.15;
  margin: 0 0 6px;
  font-variant-numeric: tabular-nums;        /* 表格等宽数字 */
  -webkit-text-fill-color: currentColor;     /* 防透明兜底 */
}
.stat-label {
  font-size: 13px;
  color: var(--ios-muted, #8e8e93);
  margin-top: 2px;
  opacity: .72;
}
.stat-value-small {
  font-size: 14px;
  color: var(--ios-fg, #1c1c1e);
  opacity: .78;
}
html.dark .stat-value-small { color: var(--ios-fg, #f5f5f7); }
/* ===== 悬浮/按下效果：卡片浮起、阴影增强（仅鼠标设备启用） ===== */
.stat-item,
.stat-item-full {
  transition: transform .22s ease, box-shadow .22s ease, border-color .22s ease;
  will-change: transform, box-shadow;
}

/* 仅在可 hover 的环境启用（避免移动端误触） */
@media (hover: hover) and (pointer: fine) {
  .stat-item:hover {
    transform: translateY(-3px) !important;
    box-shadow: 0 14px 38px rgba(0,0,0,0.10), 0 4px 16px rgba(0,0,0,0.06) !important;
    border-color: rgba(0,0,0,0.10) !important;
  }
  .stat-item:active {
    transform: translateY(-1px) !important; /* 轻压感 */
    box-shadow: 0 10px 28px rgba(0,0,0,0.08), 0 3px 12px rgba(0,0,0,0.05) !important;
  }

  /* 信息卡悬浮弱一点，保持层级克制 */
  .stat-item-full:hover {
    transform: translateY(-2px) !important;
    box-shadow: 0 12px 32px rgba(0,0,0,0.09), 0 3px 14px rgba(0,0,0,0.05) !important;
    border-color: rgba(0,0,0,0.10) !important;
  }
  .stat-item-full:active {
    transform: translateY(-1px) !important;
    box-shadow: 0 10px 24px rgba(0,0,0,0.08), 0 3px 10px rgba(0,0,0,0.05) !important;
  }

  /* 暗色模式下的悬浮阴影与边线 */
  html.dark .stat-item:hover,
  html.dark .stat-item-full:hover {
    box-shadow: 0 18px 44px rgba(0,0,0,0.6), 0 4px 16px rgba(0,0,0,0.42) !important;
    border-color: rgba(255,255,255,0.16) !important;
  }
  html.dark .stat-item:active,
  html.dark .stat-item-full:active {
    box-shadow: 0 14px 36px rgba(0,0,0,0.55), 0 3px 12px rgba(0,0,0,0.38) !important;
  }
}

/* 尊重减少动效偏好 */
@media (prefers-reduced-motion: reduce) {
  .stat-item,
  .stat-item-full {
    transition: none;
  }
}

/* 全局变量兜底（scoped 中用 :global 保证能作用到根） */
:global(:root){
  --ios-fg: #1c1c1e;               /* 亮色文字 */
  --ios-muted: #8e8e93;            /* 亮色次级文字 */
}

:global(html.dark){
  --ios-fg: #f5f5f7;               /* 暗色主文字 */
  --ios-muted: rgba(235,235,245,.6); /* 暗色次级文字 */
}

/* 防止之前的透明文本规则影响数值颜色 */
.stat-value{ -webkit-text-fill-color: currentColor; }

/* 按钮/小字在暗色下也要能看清（可选加强） */
:global(html.dark) .stat-value-small{ color: var(--ios-fg); }
:global(html.dark) .stat-label{ color: var(--ios-muted); }
:global(html.dark) .error-message button{
  color: var(--ios-fg);
  background: rgba(118,118,128,.24);
  border-color: rgba(255,255,255,.12);
}

</style>

