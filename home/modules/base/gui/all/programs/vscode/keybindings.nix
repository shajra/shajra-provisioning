# See https://vspacecode.github.io/docs/bonus/keybindings/
[
  # Basic operations
  {
    key = "ctrl+c";
    command = "editor.action.clipboardCopyAction";
    when = "textInputFocus";
  }
  {
    key = "ctrl+v";
    command = "editor.action.clipboardPasteAction";
    when = "textInputFocus && !editorReadonly";
  }
  {
    key = "ctrl+x";
    command = "editor.action.clipboardCutAction";
    when = "textInputFocus && !editorReadonly";
  }
  {
    key = "ctrl+s";
    command = "workbench.action.files.save";
  }
  {
    key = "ctrl+f";
    command = "actions.find";
  }

  # Quick Window Navigation
  {
    key = "ctrl+h";
    command = "workbench.action.navigateLeft";
    when = "!inQuickOpen && !suggestWidgetVisible && !parameterHintsVisible && !isInDiffEditor";
  }
  {
    key = "ctrl+j";
    command = "workbench.action.navigateDown";
    when = "!codeActionMenuVisible && !inQuickOpen && !suggestWidgetVisible && !parameterHintsVisible";
  }
  {
    key = "ctrl+k";
    command = "workbench.action.navigateUp";
    when = "!codeActionMenuVisible && !inQuickOpen && !suggestWidgetVisible && !parameterHintsVisible";
  }
  {
    key = "ctrl+l";
    command = "workbench.action.navigateRight";
    when = "!codeActionMenuVisible && !inQuickOpen && !suggestWidgetVisible && !parameterHintsVisible && !isInDiffEditor";
  }

  # Quick Navigation for diff view
  {
    key = "ctrl+h";
    command = "workbench.action.compareEditor.focusSecondarySide";
    when = "isInDiffEditor && !isInDiffLeftEditor";
  }
  {
    key = "ctrl+h";
    command = "workbench.action.navigateLeft";
    when = "isInDiffEditor && isInDiffLeftEditor";
  }
  {
    key = "ctrl+l";
    command = "workbench.action.compareEditor.focusPrimarySide";
    when = "isInDiffEditor && isInDiffLeftEditor";
  }
  {
    key = "ctrl+l";
    command = "workbench.action.navigateRight";
    when = "isInDiffEditor && !isInDiffLeftEditor";
  }

  # Easy List Navigation
  {
    key = "ctrl+h";
    command = "list.collapse";
    when = "listFocus && !inputFocus";
  }
  {
    key = "ctrl+l";
    command = "list.expand";
    when = "listFocus && !inputFocus";
  }
  {
    key = "ctrl+j";
    command = "list.focusDown";
    when = "listFocus && !inputFocus";
  }
  {
    key = "ctrl+k";
    command = "list.focusUp";
    when = "listFocus && !inputFocus";
  }

  # File Browser Actions
  {
    key = "ctrl+a";
    command = "-file-browser.actions";
    when = "inFileBrowser";
  }
  {
    key = "ctrl+o";
    command = "file-browser.actions";
    when = "inFileBrowser";
  }
]