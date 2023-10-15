from pygments.style import Style
from pygments.token import Comment, Keyword, Name, String, Number, Operator

class Colors(Style):
    styles = {
            Keyword: 'ansibrightblue',
            Name.Builtin: 'ansibrightcyan',
            Name.Function: 'ansibrightyellow',
            String: 'ansibrightmagenta',
            Number: 'ansibrightyellow',
            Comment: 'ansigray',
    }

# Get jupyter config object
c = get_config()

# Do not confirm exit, just exit
c.TerminalInteractiveShell.confirm_exit = False

# Disable autosuggestion (that uses history items)
c.TerminalInteractiveShell.autosuggestions_provider = None

# Shell color scheme
c.TerminalInteractiveShell.colors = 'NoColor'

# Syntax highlighting color scheme
c.TerminalInteractiveShell.highlighting_style = Colors

