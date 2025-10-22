-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typst" },
  { import = "astrocommunity.media.img-clip-nvim" },
  { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.motion.nvim-surround" },
  { import = "astrocommunity.pack.lean" },
  { import = "astrocommunity.pack.bash" },
  { import = "astrocommunity.pack.haskell" },
  { import = "astrocommunity.pack.python" },
  { import = "astrocommunity.lsp.lsp-signature-nvim" },
  { import = "astrocommunity.lsp.lsp-lens-nvim" },
  { import = "astrocommunity.file-explorer.telescope-file-browser-nvim" },
  { import = "astrocommunity.fuzzy-finder.telescope-nvim" },

  --{ import = "astrocommunity.recipes.astrolsp-auto-signature-help" },

  -- import/override with your plugins folder
}
