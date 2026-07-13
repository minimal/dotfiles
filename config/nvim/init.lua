vim.cmd.runtime("vimrc.vim")

require("oil").setup({
  columns = {
    "icon",
    "permissions",
    -- "size",
    -- "mtime",
  },
})
