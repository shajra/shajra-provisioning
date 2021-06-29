pkgs:

{
    enable = true;
    plugins = with pkgs.vimPlugins; [
        vim-grammarous
        vim-polyglot
    ];
    extraConfig = ''
        set background=light
        set expandtab
        set modeline
        set shiftwidth=4
        set tabstop=4
        set textwidth=80
        hi Search ctermbg=LightGrey
        autocmd BufWritePre * :%s/\s\+$//e
        let g:grammarous#languagetool_cmd='${pkgs.languagetool}/bin/languagetool-commandline'
        nmap <leader>x <Plug>(grammarous-close-info-window)
        nmap <c-n> <Plug>(grammarous-move-to-next-error)
        nmap <c-p> <Plug>(grammarous-move-to-previous-error)
    '';
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
}
