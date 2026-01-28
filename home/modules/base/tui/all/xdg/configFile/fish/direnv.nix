{
  cacheHome,
  coreutils,
  direnv,
  gnused,
}:

''
  ${direnv}/bin/direnv hook fish \
      | ${gnused}/bin/sed 's/^.*export fish.*$/__direnv_export_fish/' \
      | source

  function __direnv_export_fish
      if set --query __direnv_hook_enabled
          __direnv_export_fish_unsourced | source
      end
  end

  function __direnv_export_fish_unsourced
      ${direnv}/bin/direnv export fish
  end

  function direnv-toggle \
      --description "toggle Direnv on/off"
      if set --query __direnv_hook_enabled
          direnv-disable
      else
          direnv-enable
      end
  end

  function direnv-status \
      --description "see if Direnv is on or off"
      if set --query __direnv_hook_enabled
          echo Direnv is enabled
      else
          echo Direnv is disabled
      end
  end

  function direnv-retry \
      --description "cycle Direnv off, then on"
      if set --query __direnv_hook_enabled
          direnv-disable
      end
      direnv-enable
  end

  function direnv-debug-toggle \
      --description "toggle Direnv debug output on/off"
      if set --query __direnv_hook_debug
          echo "direnv: turning off debug output"
          set --erase __direnv_hook_debug
      else
          echo "direnv: turning on debug output"
          set --global __direnv_hook_debug 0
      end
  end

  function direnv-disable \
      --description "turn off Direnv"
      set dir ${cacheHome}
      ${coreutils}/bin/mkdir -p $dir/direnv/empty
      direnv-freeze $dir/direnv/empty
  end

  function direnv-freeze \
      --description "freeze Direnv environment for a directory"
      if test (count $argv) -eq 1
          if test -e $argv[1]
              echo "direnv: setting up shell environment for directory $argv[1]"
          end
          pushd $argv[1] >/dev/null; or return 1
          __direnv_export_fish_unsourced | source
          set --erase __direnv_hook_enabled
          popd >/dev/null
      end
      echo "direnv: disabling shell hook"
      set --erase __direnv_hook_enabled
  end

  function direnv-thaw \
      --description "return frozen Direnv to normal (enabled) operation"
      echo "direnv: enabling shell hook"
      set --global __direnv_hook_enabled 0
  end

  function direnv-enable \
      --description "turn on Direnv"
      direnv-thaw
  end
''
