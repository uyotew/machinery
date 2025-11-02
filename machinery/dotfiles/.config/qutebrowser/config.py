# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

# Force a Qt platform to use. This sets the `QT_QPA_PLATFORM`
# environment variable and is useful to force using the XCB plugin when
# running QtWebEngine on Wayland.
# Type: String
#c.qt.force_platform = 'wayland'

config.set('content.cookies.accept', 'all', 'chrome-devtools://*')

config.set('content.cookies.accept', 'all', 'devtools://*')

config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')

config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')

# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0', 'https://accounts.google.com/*')

# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'chrome-devtools://*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome-devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# Allow websites to show notifications.
# Type: BoolAsk
# Valid values:
#   - true
#   - false
#   - ask
config.set('content.notifications.enabled', False, 'https://www.reddit.com')

# Height (in pixels or as percentage of the window) of the completion.
# Type: PercOrInt
c.completion.height = '25%'

c.statusbar.widgets = ['keypress', 'url', 'scroll', 'history', 'progress']

# Mouse button with which to close tabs.
# Type: String
# Valid values:
#   - right: Close tabs on right-click.
#   - middle: Close tabs on middle-click.
#   - none: Don't close tabs using the mouse.
c.tabs.close_mouse_button = 'none'

c.tabs.show = 'never'

# Open a new window for every tab.
# Type: Bool
c.tabs.tabs_are_windows = True

# To avoid getting flashbanged
c.colors.webpage.bg = '#theme-background'

c.colors.webpage.preferred_color_scheme = theme-light?'light':'dark';
c.colors.webpage.darkmode.enabled = theme-dark?True:False;

c.colors.completion.category.bg = '#theme-secondary'
c.colors.completion.category.fg = '#theme-background'
c.colors.completion.category.border.top = '#theme-secondary'
c.colors.completion.category.border.bottom = '#theme-secondary'

c.colors.completion.even.bg = '#theme-background'
c.colors.completion.odd.bg = '#theme-soft-background'
c.colors.completion.fg = '#theme-text'
c.colors.completion.match.fg = '#theme-tertiary'

c.colors.completion.item.selected.bg = '#theme-primary'
c.colors.completion.item.selected.fg = '#theme-background'
c.colors.completion.item.selected.border.top = '#theme-primary'
c.colors.completion.item.selected.border.bottom = '#theme-primary'
c.colors.completion.item.selected.match.fg = '#theme-tertiary'

c.colors.completion.scrollbar.bg = '#theme-soft-background'
c.colors.completion.scrollbar.fg = '#theme-text'

c.colors.downloads.bar.bg = '#theme-background'
c.colors.downloads.start.bg = '#theme-secondary'
c.colors.downloads.start.fg = '#theme-background'
c.colors.downloads.stop.bg = '#theme-primary'
c.colors.downloads.stop.fg = '#theme-background'

c.colors.hints.bg = '#e0theme-primary' # slightly transparent
c.colors.hints.fg = '#theme-background'
c.colors.hints.match.fg = '#theme-text'
c.hints.border = '0px'
c.hints.radius = 0

c.colors.keyhint.bg = '#70theme-soft-background' # slightly transparent
c.colors.keyhint.fg = '#theme-text'
c.colors.keyhint.suffix.fg = '#theme-tertiary'

c.colors.messages.info.bg = '#theme-background'
c.colors.messages.info.fg = '#theme-text'
c.colors.messages.info.border = '#theme-background'

c.colors.prompts.bg = '#theme-soft-background'
c.colors.prompts.fg = '#theme-text'
c.colors.prompts.border = '2px solid #theme-primary'
c.colors.prompts.selected.bg = '#theme-primary'
c.colors.prompts.selected.fg = '#theme-soft-background'

c.colors.statusbar.caret.bg = '#theme-tertiary'
c.colors.statusbar.caret.fg = '#theme-background'
c.colors.statusbar.caret.selection.bg = '#theme-primary'
c.colors.statusbar.caret.selection.fg = '#theme-background'
c.colors.statusbar.command.bg = '#theme-soft-background'
c.colors.statusbar.command.fg = '#theme-text'
c.colors.statusbar.insert.bg = '#theme-secondary'
c.colors.statusbar.insert.fg = '#theme-background'
c.colors.statusbar.normal.bg = '#theme-soft-background'
c.colors.statusbar.normal.fg = '#theme-text'
c.colors.statusbar.passthrough.bg = '#theme-soft'
c.colors.statusbar.passthrough.fg = '#theme-background'
c.colors.statusbar.progress.bg = '#theme-text'
c.colors.statusbar.url.fg = '#theme-text'
c.colors.statusbar.url.hover.fg = '#theme-tertiary'
c.colors.statusbar.url.success.http.fg = '#theme-text'
c.colors.statusbar.url.success.https.fg = '#theme-text'

c.colors.tooltip.bg = '#theme-soft-background'
c.colors.tooltip.fg = '#theme-soft'

c.content.user_stylesheets = ['~/.config/qutebrowser/user.css']


# Bindings for normal mode
config.unbind('D')
config.unbind('d')
config.unbind('<Ctrl-h>')


config.unbind('u')
scripts = str(config.configdir) + '/userscripts/'
config.bind('uo','spawn --userscript ' + scripts + 'original')
config.bind('un','spawn --userscript ' + scripts + 'nhd')
config.bind('ur','spawn --userscript ' + scripts + 'rd')

c.hints.selectors["every"] = ["*"]

config.bind(';a', 'hint every')

config.bind('K', 'scroll-px 0 -150')
config.bind('J', 'scroll-px 0 150')

config.bind(';v', 'hint links spawn -d mpv {hint-url}')
config.bind(',v', 'spawn -d mpv {url}')
config.bind(';V', 'hint links spawn -d mpv --ytdl-raw-options-add=cookies-from-browser=chromium:~/.local/share/qutebrowser/ {hint-url}')
config.bind(',V', 'spawn -d mpv --ytdl-raw-options-add=cookies-from-browser=chromium:~/.local/share/qutebrowser/ {url}')

config.bind('*', 'mode-enter set_mark')

