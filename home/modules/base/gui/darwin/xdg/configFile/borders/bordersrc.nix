colors:

''
#!/bin/bash

options=(
	style=round
	width=8.0
	hidpi=on
	active_color=${colors.window.selected.focused.border.window}
	inactive_color=${colors.window.unselected.border.window}
)

borders "''${options[@]}"
''
