import { resolve } from 'path'
import { defineConfig } from 'vite'
import { plugin } from 'vite-plugin-elm'

export default defineConfig({
  plugins: [plugin()],
  build: {
    outDir: "docs",
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'index.html'),
      },
    },
  },
})
