config: pkgs:

let

  font = builtins.replaceStrings [ " " ] [ "\\ " ] config.theme.fonts.monospaced.code.name;
  kitty-scrollback = pkgs.vimUtils.buildVimPlugin {
    name = "kitty-scrollback.nvim";
    src = pkgs.sources.kitty-scrollback-nvim;
  };

in
{
  enable = true;
  plugins =
    with pkgs.vimPlugins;
    [
      fzf-vim
      kitty-scrollback
      vim-grammarous
      vim-polyglot
    ]
    ++ config.theme.external.neovim.plugins;
  extraConfig = ''
    set background=light
    set expandtab
    set guifont=${font}
    set modeline
    set shiftwidth=4
    set tabstop=4
    set textwidth=80
    autocmd BufWritePre * :%s/\s\+$//e
    let g:grammarous#languagetool_cmd='${pkgs.languagetool}/bin/languagetool-commandline'
    nmap <leader>x <Plug>(grammarous-close-info-window)
    nmap <c-n> <Plug>(grammarous-move-to-next-error)
    nmap <c-p> <Plug>(grammarous-move-to-previous-error)
    if exists("g:neovide")
        nnoremap <C-=> :let g:neovide_scale_factor += 0.1<CR>
        nnoremap <C--> :let g:neovide_scale_factor -= 0.1<CR>
        nnoremap <C-0> :let g:neovide_scale_factor = 1<CR>
        vnoremap <C-=> <Esc>:let g:neovide_scale_factor += 0.1<CR>gv
        vnoremap <C--> <Esc>:let g:neovide_scale_factor -= 0.1<CR>gv
        vnoremap <C-0> <Esc>:let g:neovide_scale_factor = 1<CR>gv
    endif

  '';
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
}
