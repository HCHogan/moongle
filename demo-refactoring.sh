#!/bin/bash
# Moongle Development Setup Script
# This script demonstrates the improved development workflow

echo "🚀 Moongle 重构演示"
echo "===================="

echo "📁 查看重构后的文件结构:"
echo "前端模块化:"
find web/plugins/moonbit-build -name "*.ts" | sort
echo
find web/src/components/highlighter -name "*.ts" -o -name "*.vue" | sort

echo
echo "📁 后端改进:"
find src/Moongle -name "*.hs" | grep -E "(Error|Enhanced)" | sort

echo
echo "📚 文档和配置:"
ls -la REFACTORING.md config.example.yaml

echo
echo "✅ 重构亮点:"
echo "1. Vite Plugin: 200+ 行大函数 → 模块化结构"
echo "2. Vue 组件: 混合逻辑 → Composition API + 工具函数"
echo "3. Haskell: 通用错误 → 类型化错误处理"
echo "4. 配置: 扁平结构 → 分层配置 + 验证"
echo "5. 文档: 完整的重构指南和迁移策略"

echo
echo "🎯 使用重构后的组件:"
echo "// 替换原来的插件"
echo "import moonbitTreeSitterPlugin from './plugins/vite-plugin-moonbit-treesitter-refactored'"
echo
echo "// 使用新的 Vue 组件"
echo "<CodeHighlighterRefactored :code=\"sourceCode\" :show-stats=\"true\" />"

echo
echo "🔧 Haskell 后端改进:"
echo "-- 类型化错误处理"
echo "loadConfig :: IO (Either MoongleError Config)"
echo "-- 结构化配置"
echo "config ^. database . dbHost"

echo
echo "✨ 重构完成！代码质量和可维护性显著提升"