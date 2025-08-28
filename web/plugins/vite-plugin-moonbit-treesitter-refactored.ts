import type { Plugin } from 'vite'
import { resolve } from 'node:path'
import { MoonbitBuildManager } from './moonbit-build/manager'
import type { MoonbitTsOptions } from './moonbit-build/types'

/**
 * Vite plugin for Moonbit Tree-sitter integration
 * 
 * 重构后的插件，分离了关注点：
 * - 插件主体只负责 Vite 生命周期集成
 * - 构建逻辑委托给 MoonbitBuildManager
 * - 文件操作和工具函数分离到独立模块
 */
export default function moonbitTreeSitterPlugin(opts: MoonbitTsOptions = {}): Plugin {
  const buildManager = new MoonbitBuildManager(opts)

  return {
    name: 'vite-plugin-moonbit-treesitter',

    configResolved(config) {
      const viteRoot = config.root || process.cwd()
      const publicDir = (config.publicDir as string) || resolve(viteRoot, 'public')
      const isDev = config.command === 'serve'
      
      buildManager.configResolved(viteRoot, publicDir, isDev)
    },

    configureServer(server) {
      buildManager.configureServer(server)
    },

    async buildStart() {
      await buildManager.buildStart()
    }
  }
}

// Re-export types for convenience
export type { MoonbitTsOptions }