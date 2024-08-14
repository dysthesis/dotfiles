return {
  -- Stolen from https://github.com/LazyVim/LazyVim/blob/530e94a9fa19577401e968a9673282c3d79f01e3/lua/lazyvim/plugins/extras/lang/rust.lua
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      {
        'Saecki/crates.nvim',
        event = { 'BufRead Cargo.toml' },
        opts = {
          completion = {
            cmp = { enabled = true },
          },
        },
      },
    },
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      opts.sources = opts.sources or {}
      table.insert(opts.sources, { name = 'crates' })
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'ron', 'rust', 'toml' })
    end,
  },
  {
    'williamboman/mason.nvim',
    dependencies = { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'codelldb', 'rust-analyzer' })
      require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }
    end,
  },
  {
    'mrcjkb/rustaceanvim',
    version = '^4', -- Recommended
    dependencies = {},
    init = function()
      -- Configure rustaceanvim here
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(_, b)
            vim.lsp.inlay_hint.enable(true, { bufnr = b })
          end,
          settings = {
            ['rust-analyzer'] = {
              cargo = { features = 'all' },
              assist = {
                importEnforceGranularity = true,
                importPrefix = 'crate',
              },
              checkOnSave = {
                enable = true,
                command = 'clippy',
                features = 'all',
              },
              inlayHints = {
                chainingHints = {
                  bindingModeHints = {
                    enable = true,
                  },
                  chainingHints = {
                    enable = true,
                  },
                  closingBraceHints = {
                    enable = true,
                    minLines = 25,
                  },
                  closureCaptureHints = {
                    enable = true,
                  },
                  closureReturnTypeHints = {
                    enable = 'always', -- "never"
                  },
                  closureStyle = 'impl_fn',
                  discriminantHints = {
                    enable = 'always', -- "never"
                  },
                  expressionAdjustmentHints = {
                    hideOutsideUnsafe = false,
                    mode = 'prefix',
                  },
                  implicitDrops = {
                    enable = true,
                  },
                  lifetimeElisionHints = {
                    enable = 'always', -- "never"
                    useParameterNames = true,
                  },
                  maxLength = 25,
                  parameterHints = {
                    enable = true,
                  },
                  rangeExclusiveHints = {
                    enable = true,
                  },
                  renderColons = {
                    enable = true,
                  },
                  typeHints = {
                    enable = true,
                    hideClosureInitialization = false,
                    hideNamedConstructor = false,
                  },
                },
              },
              lens = {
                enable = true,
              },
            },
          },
          standalone = true,
          -- cmd = { "rustup", "run", "stable", "rust-analyzer" },
          cmd = { '/usr/bin/rust-analyzer' },
        },
        -- DAP configuration
        dap = {},
      }
    end,
    ft = { 'rust' },
  },
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        rust_analyzer = {},
        taplo = {
          keys = {
            {
              'K',
              function()
                if vim.fn.expand '%:t' == 'Cargo.toml' and require('crates').popup_available() then
                  require('crates').show_popup()
                else
                  vim.lsp.buf.hover()
                end
              end,
              desc = 'Show Crate Documentation',
            },
          },
        },
      },
      setup = {
        rust_analyzer = function()
          return true
        end,
      },
    },
  },
  {
    'nvim-neotest/neotest',
    optional = true,
    opts = function(_, opts)
      opts.adapters = opts.adapters or {}
      vim.list_extend(opts.adapters, {
        require 'rustaceanvim.neotest',
      })
    end,
  },
}
