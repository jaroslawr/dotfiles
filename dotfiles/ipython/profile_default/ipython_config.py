from IPython.utils.PyColorize import Theme, theme_table
from pygments.token import Comment, Keyword, Name, String, Number, Operator

theme_table["mycolors"] = Theme(
    "mycolors",
    None,
    {
            Keyword: 'ansiblue',
            Name.Builtin: 'ansicyan',
            Name.Function: 'ansiyellow',
            String: 'ansimagenta',
            Number: 'ansired',
            Comment: 'ansibrightblack',
    }
)

# Get jupyter config object
c = get_config()

# Do not confirm exit, just exit
c.TerminalInteractiveShell.confirm_exit = False

# Disable autosuggestion (that uses history items)
c.TerminalInteractiveShell.autosuggestions_provider = None

# Syntax highlighting color scheme
c.TerminalInteractiveShell.colors = "mycolors"

