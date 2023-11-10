from pygments.style import Style
from pygments.token import Comment, Keyword, Name, String, Number, Operator

class Colors(Style):
    styles = {
            Keyword: 'ansiblue',
            Name.Builtin: 'ansicyan',
            Name.Function: 'ansiyellow',
            String: 'ansimagenta',
            Number: 'ansired',
            Comment: 'ansibrightblack',
    }

# Get jupyter config object
c = get_config()

# Do not confirm exit, just exit
c.TerminalInteractiveShell.confirm_exit = False

# Disable autosuggestion (that uses history items)
c.TerminalInteractiveShell.autosuggestions_provider = None

# Syntax highlighting color scheme
c.TerminalInteractiveShell.highlighting_style = Colors

