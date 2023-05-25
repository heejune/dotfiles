--[[ init.lua ]]
-- from https://github.com/rsdlt/nvim-config-files
-- https://rsdlt.github.io/posts/rust-nvim-ide-guide-walkthrough-development-debug/

-- Impatient for faster loading
require('impatient')

-- LEADER
vim.g.mapleader = " " -- works across all nvim files

-- Color theme
require('kanagawa').setup({
    keywordStyle = { italic = false, bold = true },
})
vim.cmd("colorscheme kanagawa")

-- Highlight colors
vim.cmd([[
hi CursorLineNr guifg=#7e9cd8
" hi FoldColumn guifg=#4a4f54 guibg=#26292c
hi FoldColumn guifg=#29292c guibg=#26292c
hi GitSignsAdd guibg=#193549 guifg=#3ad900
hi GitSignsChange guibg=#193549 guifg=#ffc600
hi GitSignsDelete guibg=#193549 guifg=#ff2600
hi ColorColumn guifg=NONE guibg=#204563 gui=NONE
]])

-- IMPORTS
require('vars') -- Variables
require('opts') -- Options
require('keys') -- Keymaps
require('plug') -- Plugins

-- Lua line
require('lualine').setup {
    options = {
        theme = 'kanagawa',
        fmt = string.lower,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
    }
}

----------------------------------------
-- PLUGINS initialization           ----
----------------------------------------


-- Symbols Outline (new tagbar)
require("symbols-outline").setup {
    show_guides = false,
    position = 'left',
    width = 35,
    wrap = false,
    keymaps = { fold = "h", unfold = "l", fold_all = "zM", unfold_all = "zR" },
    symbols = {
        File = { icon = "", hl = "TSURI" },
        Module = { icon = "全", hl = "TSNamespace" },
        Namespace = { icon = "凜", hl = "TSNamespace" },
        Package = { icon = "", hl = "TSNamespace" },
        Class = { icon = "", hl = "TSType" },
        Method = { icon = "", hl = "TSMethod" },
        Property = { icon = "", hl = "TSMethod" },
        Field = { icon = "", hl = "TSField" },
        Constructor = { icon = "", hl = "TSConstructor" },
        Enum = { icon = "", hl = "TSType" },
        Interface = { icon = "ﰮ", hl = "TSType" },
        Function = { icon = "", hl = "TSFunction" },
        Variable = { icon = "", hl = "TSConstant" },
        Constant = { icon = "", hl = "TSConstant" },
        String = { icon = "", hl = "TSString" },
        Number = { icon = "#", hl = "TSNumber" },
        Boolean = { icon = "⊨", hl = "TSBoolean" },
        Array = { icon = "", hl = "TSConstant" },
        Object = { icon = "", hl = "TSType" },
        Key = { icon = "🔐", hl = "TSType" },
        Null = { icon = "ﳠ", hl = "TSType" },
        EnumMember = { icon = "", hl = "TSField" },
        Struct = { icon = "", hl = "TSType" },
        Event = { icon = "🗲", hl = "TSType" },
        Operator = { icon = "+", hl = "TSOperator" },
        TypeParameter = { icon = "", hl = "TSParameter" }
    },
}

-- Better escape
require("better_escape").setup {
    mapping = { "jk", "kj" }, -- a table with mappings to use
    timeout = vim.o.timeoutlen, -- the time in which the keys must be hit in ms. Use option timeoutlen by default
    clear_empty_lines = false, -- clear line after escaping if there is only whitespace
    keys = "<Esc>", -- keys used for escaping, if it is a function will use the result everytime
}


-- FTerm
require 'FTerm'.setup({
    border     = 'single',
    cmd        = os.getenv('SHELL'),
    -- cmd        = 'fish',
    blend      = 0,
    dimensions = {
        height = 0.9,
        width = 0.9,
    },
})
vim.keymap.set('n', 't', '<CMD>lua require("FTerm").toggle()<CR>')
vim.keymap.set('t', '<Esc>', '<C-\\><C-n><CMD>lua require("FTerm").toggle()<CR>')


-- null-ls for Vale, Selene and Markdown linters
require("null-ls").setup({
    sources = {
        require("null-ls").builtins.diagnostics.vale,
        require("null-ls").builtins.diagnostics.selene,
        require("null-ls").builtins.diagnostics.markdownlint.with({
            diagnostics_format = "[#{c}] #{m} (#{s})",
            filter = function(diagnostic)
                return diagnostic.code ~= "MD013/line-length"
            end,
        }),
    },
})


-- Headlines for Markdown
require "headlines".setup()


-- Fidget - show status of LSP progress
require "fidget".setup {
    window = {
        relative = "editor",
        blend = 10,
    },
}


-- Vale prose lint
require("null-ls").setup({
    sources = {
        require("null-ls").builtins.diagnostics.vale,
    },
})


-- Hop
require 'hop'.setup {
    keys = 'etovxqpdygfblzhckisuran',
    jump_on_sole_occurrence = false,
}


-- Ident Lines
vim.cmd [[highlight IndentBlanklineIndent1 guifg=#2d3033 gui=nocombine]]
vim.cmd [[highlight IndentBlanklineIndent2 guifg=#2d3033 gui=nocombine]]
vim.cmd [[highlight IndentBlanklineIndent3 guifg=#2d3033 gui=nocombine]]
vim.cmd [[highlight IndentBlanklineIndent4 guifg=#2d3033 gui=nocombine]]
vim.cmd [[highlight IndentBlanklineIndent5 guifg=#2d3033 gui=nocombine]]
vim.cmd [[highlight IndentBlanklineIndent6 guifg=#2d3033 gui=nocombine]]

require("indent_blankline").setup {
    -- char = '┊',
    char = ' ',
    use_treesitter = true,
    use_treesitter_scope = true,
    show_first_indent_level = true,
    space_char_blankline = " ",
    char_highlight_list = {
        "IndentBlanklineIndent1",
        "IndentBlanklineIndent2",
        "IndentBlanklineIndent3",
        "IndentBlanklineIndent4",
        "IndentBlanklineIndent5",
        "IndentBlanklineIndent6",
    },
}


-- Todo Comments Setup
require('todo-comments').setup {
    colors = {
        error = { "DiagnosticError", "ErrorMsg", "#de5d68" },
        warning = { "DiagnosticWarning", "WarningMsg", "#eeb927" },
        info = { "DiagnosticInfo", "#57a5e5" },
        hint = { "DiagnosticHint", "#bb70d2" },
        default = { "Identifier", "#de5d68" },
    },
}


-- Trouble Setup
require('trouble').setup {
    position = "right",
    width = 75,
    padding = true,
    auto_preview = false,
}


-- Nvim Tree Setup
local function on_attach(bufnr)
  local api = require('nvim-tree.api')

  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end


  -- default mappings. feel free to modify or remove as you wish.
  --
  -- begin_default_on_attach
  vim.keymap.set('n', '<c-]>', api.tree.change_root_to_node,          opts('cd'))
  vim.keymap.set('n', '<c-e>', api.node.open.replace_tree_buffer,     opts('open: in place'))
  vim.keymap.set('n', '<c-k>', api.node.show_info_popup,              opts('info'))
  vim.keymap.set('n', '<c-r>', api.fs.rename_sub,                     opts('rename: omit filename'))
  vim.keymap.set('n', '<c-t>', api.node.open.tab,                     opts('open: new tab'))
  vim.keymap.set('n', '<c-v>', api.node.open.vertical,                opts('open: vertical split'))
  vim.keymap.set('n', '<c-x>', api.node.open.horizontal,              opts('open: horizontal split'))
  vim.keymap.set('n', '<bs>',  api.node.navigate.parent_close,        opts('close directory'))
  vim.keymap.set('n', '<cr>',  api.node.open.edit,                    opts('open'))
  vim.keymap.set('n', '<tab>', api.node.open.preview,                 opts('open preview'))
  vim.keymap.set('n', '>',     api.node.navigate.sibling.next,        opts('next sibling'))
  vim.keymap.set('n', '<',     api.node.navigate.sibling.prev,        opts('previous sibling'))
  vim.keymap.set('n', '.',     api.node.run.cmd,                      opts('run command'))
  vim.keymap.set('n', '-',     api.tree.change_root_to_parent,        opts('up'))
  vim.keymap.set('n', 'a',     api.fs.create,                         opts('create'))
  vim.keymap.set('n', 'bmv',   api.marks.bulk.move,                   opts('move bookmarked'))
  vim.keymap.set('n', 'b',     api.tree.toggle_no_buffer_filter,      opts('toggle no buffer'))
  vim.keymap.set('n', 'c',     api.fs.copy.node,                      opts('copy'))
  vim.keymap.set('n', 'c',     api.tree.toggle_git_clean_filter,      opts('toggle git clean'))
  vim.keymap.set('n', '[c',    api.node.navigate.git.prev,            opts('prev git'))
  vim.keymap.set('n', ']c',    api.node.navigate.git.next,            opts('next git'))
  vim.keymap.set('n', 'd',     api.fs.remove,                         opts('delete'))
  vim.keymap.set('n', 'd',     api.fs.trash,                          opts('trash'))
  vim.keymap.set('n', 'e',     api.tree.expand_all,                   opts('expand all'))
  vim.keymap.set('n', 'e',     api.fs.rename_basename,                opts('rename: basename'))
  vim.keymap.set('n', ']e',    api.node.navigate.diagnostics.next,    opts('next diagnostic'))
  vim.keymap.set('n', '[e',    api.node.navigate.diagnostics.prev,    opts('prev diagnostic'))
  vim.keymap.set('n', 'f',     api.live_filter.clear,                 opts('clean filter'))
  vim.keymap.set('n', 'f',     api.live_filter.start,                 opts('filter'))
  vim.keymap.set('n', 'g?',    api.tree.toggle_help,                  opts('help'))
  vim.keymap.set('n', 'gy',    api.fs.copy.absolute_path,             opts('copy absolute path'))
  vim.keymap.set('n', 'h',     api.tree.toggle_hidden_filter,         opts('toggle dotfiles'))
  vim.keymap.set('n', 'i',     api.tree.toggle_gitignore_filter,      opts('toggle git ignore'))
  vim.keymap.set('n', 'j',     api.node.navigate.sibling.last,        opts('last sibling'))
  vim.keymap.set('n', 'k',     api.node.navigate.sibling.first,       opts('first sibling'))
  vim.keymap.set('n', 'm',     api.marks.toggle,                      opts('toggle bookmark'))
  vim.keymap.set('n', 'o',     api.node.open.edit,                    opts('open'))
  vim.keymap.set('n', 'o',     api.node.open.no_window_picker,        opts('open: no window picker'))
  vim.keymap.set('n', 'p',     api.fs.paste,                          opts('paste'))
  vim.keymap.set('n', 'p',     api.node.navigate.parent,              opts('parent directory'))
  vim.keymap.set('n', 'q',     api.tree.close,                        opts('close'))
  vim.keymap.set('n', 'r',     api.fs.rename,                         opts('rename'))
  vim.keymap.set('n', 'r',     api.tree.reload,                       opts('refresh'))
  vim.keymap.set('n', 's',     api.node.run.system,                   opts('run system'))
  vim.keymap.set('n', 's',     api.tree.search_node,                  opts('search'))
  vim.keymap.set('n', 'u',     api.tree.toggle_custom_filter,         opts('toggle hidden'))
  vim.keymap.set('n', 'w',     api.tree.collapse_all,                 opts('collapse'))
  vim.keymap.set('n', 'x',     api.fs.cut,                            opts('cut'))
  vim.keymap.set('n', 'y',     api.fs.copy.filename,                  opts('copy name'))
  vim.keymap.set('n', 'y',     api.fs.copy.relative_path,             opts('copy relative path'))
  vim.keymap.set('n', '<2-leftmouse>',  api.node.open.edit,           opts('open'))
  vim.keymap.set('n', '<2-rightmouse>', api.tree.change_root_to_node, opts('cd'))
  -- end_default_on_attach


  -- mappings migrated from view.mappings.list
  --
  -- you will need to insert "your code goes here" for any mappings with a custom action_cb
  vim.keymap.set('n', 'u', api.tree.change_root_to_parent, opts('up'))

end
require('nvim-tree').setup {
    on_attach = on_attach,
    sort_by = "case_sensitive",
    view = {
        adaptive_size = false,
    },
    renderer = {
        group_empty = true,
        icons = {
            git_placement = "after",
            glyphs = {
                git = {
                    unstaged = "-",
                    staged = "s",
                    untracked = "u",
                    renamed = "r",
                    deleted = "d",
                    ignored = "i",
                },
            },
        },
    },
    filters = {
        dotfiles = false,
    },
    git = {
        ignore = false,
    },
}


-- Telescope Setup
require('telescope').setup {
    defaults = {
        file_ignore_patterns = {
            "^./.git/",
            "^./target/",
            "LICENSE*"
        },
        layout_strategy = 'vertical',
        layout_config = { height = 0.95, width = 0.95 },
    },
    extensions = {
        media_files = {
            -- filetypes whitelist
            -- defaults to {"png", "jpg", "mp4", "webm", "pdf"}
            filetypes = { "png", "webp", "jpg", "jpeg", "ppm", "pdf" },
            find_cmd = "rg", -- find command (defaults to `fd`)
        },
    },
}
-- load extension to support preview of media files
require('telescope').load_extension('media_files');
-- Get fzf loaded and working with extension
require('telescope').load_extension('fzf')


-- Autopairs Setup
require 'nvim-autopairs'.setup {}


-- Mason Setup
require("mason").setup({
    ui = {
        icons = {
            package_installed = "",
            package_pending = "",
            package_uninstalled = "",
        },
    }
})
require("mason-lspconfig").setup()


-- Comment Setup
require('Comment').setup({
    mappings = {
        basic = false,
        extra = false,
        extended = false,
    },
})

-- Crates Nvim
require('crates').setup({

})

----------------------------------------
-- LSP Server Configurations        ----
----------------------------------------

-- LSP Config
local nvim_lsp = require 'lspconfig'


-- RUST
-- -------------------------------------
local rt = require("rust-tools")

rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- Hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- Code action groups
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})



-- LUA
-- -------------------------------------
require 'lspconfig'.lua_ls.setup {
    settings = {
        Lua = {
            runtime = {
                version = 'Lua 5.4',
                path = {
                    '?.lua',
                    '?/init.lua',
                    vim.fn.expand '~/.luarocks/share/lua/5.4/?.lua',
                    vim.fn.expand '~/.luarocks/share/lua/5.4/?/init.lua',
                    '/usr/share/5.4/?.lua',
                    '/usr/share/lua/5.4/?/init.lua'
                }
            },
            workspace = {
                library = {
                    vim.fn.expand '~/.luarocks/share/lua/5.4',
                    '/usr/share/lua/5.4'
                }
            }
        }
    }
}



-- Marksman
require 'lspconfig'.marksman.setup {
}




----------------------------------------
-- COMPLETION Setup                 ----
----------------------------------------

require('lspkind').init({
    -- mode = 'symbol_text'
})


vim.opt.completeopt = { 'menuone', 'noselect', 'noinsert' }
vim.opt.shortmess = vim.opt.shortmess + { c = true }
vim.api.nvim_set_option('updatetime', 350)

local lspkind = require('lspkind')
local cmp = require 'cmp'

cmp.setup({
    -- Enable LSP snippets
    snippet = {
        expand = function(args)
            -- vim.fn["vsnip#anonymous"](args.body) -- For 'vsnip' users.
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        end,
    },
    mapping = {
        -- Add tab support
        ['<S-Tab>'] = cmp.mapping.select_prev_item(),
        ['<Tab>'] = cmp.mapping.select_next_item(),
        ['<C-S-f>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm { select = false }
    },
    -- Installed sources:
    sources = {
        { name = 'path' }, -- file paths
        { name = 'nvim_lsp', keyword_length = 1, priority = 10 }, -- from language server
        { name = 'crates', keyword_length = 1, priority = 10 },
        { name = 'luasnip', keyword_length = 1, priority = 7 }, -- for lua users
        { name = 'nvim_lsp_signature_help', priority = 8 }, -- display function signatures with current parameter emphasized
        { name = 'nvim_lua', keyword_length = 1, priority = 8 }, -- complete neovim's Lua runtime API such vim.lsp.*
        { name = 'buffer', keyword_length = 1, priority = 5 }, -- source current buffer
        -- { name = 'vsnip', keyword_length = 2 },         -- nvim-cmp source for vim-vsnip
        { name = 'calc' }, -- source for math calculation
    },
    window = {
        completion = {
            cmp.config.window.bordered(),
            col_offset = 3,
            side_padding = 1,
        },
        documentation = cmp.config.window.bordered(),

    },
    formatting = {
        fields = { 'menu', 'abbr', 'kind' },
        format = lspkind.cmp_format({
            mode = 'symbol_text', -- show only symbol annotations
            maxwidth = 60, -- prevent the popup from showing more than provided characters
            -- The function below will be called before any actual modifications from lspkind:
            before = function(entry, vim_item)
                local menu_icon = {
                    nvim_lsp = 'λ ',
                    luasnip = '⋗ ',
                    buffer = 'Ω ',
                    path = '🖫 ',
                }
                vim_item.menu = menu_icon[entry.source.name]
                return vim_item
            end,
        })

    },
    preselect = cmp.PreselectMode.None,
    confirmation = {
        get_commit_characters = function(commit_characters)
            return {}
        end
    },
})

-- `/` cmdline setup.
cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    }
})
-- `:` cmdline setup.
cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})



----------------------------------------
-- TREE-SITTER Setup                ----
----------------------------------------

require('nvim-treesitter.configs').setup {
    ensure_installed = { "bash", "c", "cmake", "css", "dockerfile", "go", "gomod", "gowork", "hcl", "help", "html",
        "http", "javascript", "json", "lua", "make", "markdown", "python", "regex", "ruby", "rust", "toml", "vim", "yaml",
        "zig" },
    auto_install = true,
    highlight = {
        enable = true,
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<S-Tab>", -- normal mode
            node_incremental = "<Tab>", -- visual mode
            node_decremental = "<S-Tab", -- visual mode
        },
    },
    ident = { enable = true },
    rainbow = {
        enable = true,
    }
}


------------------------------------
-- Nvim UFO - GOOD FOLDING      ----
--https://github.com/kevinhwang91/nvim-ufo
-- ---------------------------------
vim.cmd([[
augroup MyColors
autocmd!
augroup end
]])

vim.o.foldcolumn = '1'
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true
-- Option 3: treesitter as a main provider instead
-- Only depend on `nvim-treesitter/queries/filetype/folds.scm`,
-- performance and stability are better than `foldmethod=nvim_treesitter#foldexpr()`
-- require('ufo').setup({
--     provider_selector = function(bufnr, filetype, buftype)
--         return {'treesitter', 'indent'}
--     end
-- })
local handler = function(virtText, lnum, endLnum, width, truncate)
    local newVirtText = {}
    local suffix = ('   %d '):format(endLnum - lnum)
    local sufWidth = vim.fn.strdisplaywidth(suffix)
    local targetWidth = width - sufWidth
    local curWidth = 0
    for _, chunk in ipairs(virtText) do
        local chunkText = chunk[1]
        local chunkWidth = vim.fn.strdisplaywidth(chunkText)
        if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
        else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
                suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
            end
            break
        end
        curWidth = curWidth + chunkWidth
    end
    table.insert(newVirtText, { suffix, 'MoreMsg' })
    return newVirtText
end

-- global handler
require('ufo').setup({
    fold_virt_text_handler = handler,

    provider_selector = function(bufnr, filetype, buftype)
        return { 'treesitter', 'indent' }
    end
})
-- buffer scope handler
-- will override global handler if it is existed
local bufnr = vim.api.nvim_get_current_buf()
require('ufo').setFoldVirtTextHandler(bufnr, handler)

-- Hide foldcolumn for transparency
vim.opt.foldcolumn = '0'


------------------------------------
-- DIFFVIEW                     ----
-- -https://github.com/sindrets/diffview.nvim
-- --------------------------------
-- Lua
local actions = require("diffview.actions")

require("diffview").setup({
})
