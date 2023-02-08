c = get_config()

# Shell color scheme
c.TerminalInteractiveShell.colors = 'NoColor'

# Syntax highlighting color scheme
c.TerminalInteractiveShell.highlighting_style = 'monokai'

# Disable autosuggestion (that uses history items)
c.TerminalInteractiveShell.autosuggestions_provider = None

# Do not confirm exit, just exit
c.TerminalInteractiveShell.confirm_exit = False
