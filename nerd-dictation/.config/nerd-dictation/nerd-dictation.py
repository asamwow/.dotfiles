# User configuration file typically located at `~/.config/nerd-dictation/nerd-dictation.py`
import re

# -----------------------------------------------------------------------------
# Replace Multiple Words

TEXT_REPLACE_REGEX = (
    ("\\b" "key word" "\\b", "keyword"),
    ("\\b" "for each" "\\b", "foreach"),
    ("\\b" "new line" "\\b", "newline"),
    ("\\b" "right boy" "\\b", ")"),
    ("\\b" "right bracket" "\\b", "]"),
    ("\\b" "right curly" "\\b", "}"),
    ("\\b" "x ray" "\\b", "x"),
    ("\\b" "exclamation point" "\\b", "!"),
    ("\\b" "question mark" "\\b", "?"),
    ("\\b" "less than" "\\b", "<"),
    ("\\b" "greater than" "\\b", ">"),
)
TEXT_REPLACE_REGEX = tuple(
    (re.compile(match), replacement)
    for (match, replacement) in TEXT_REPLACE_REGEX
)


# -----------------------------------------------------------------------------
# Replace Single Words

# VOSK-API doesn't use capitals anywhere so they have to be explicit added in.
WORD_REPLACE = {
    "i": "I",
    "api": "API",
    "linux": "Linux",
    "get": "git",
    "the": "", # HACK
    "and": "end",
    "diary": "dired",

    # symbols
    "space": " ",
    "dash": "-",
    "quotes": "\"",
    "perrin": "(",
    "bracket": "[",
    "curly": "{",
    "slash": "/",
    "colon": ":",
    "apostrophe": "'",
    "comma": ",",
    "semi": ";",
    "dot": ".",
    "tilda": "~",
    "tick": "`",
    "equals": "=",
    "asterisk": "*",
    "carrot": "^",
    "amp": "&",
    "pipe": "|",
    "at": "@",
    "hash": "#",
    "percent": "%",

    # nato phonetic
    "alpha": "a",
    "bravo": "b",
    "charlie": "c",
    "delta": "d",
    "echo": "e",
    "foxtrot": "f",
    "golf": "g",
    "gulf": "g",
    "hotel": "h",
    "india": "i",
    "juliet": "j",
    "kilos": "k",
    "kilo": "k",
    "lima": "l",
    "mike": "m",
    "november": "n",
    "oscar": "o",
    "papa": "p",
    "quebec": "q",
    "romeo": "r",
    "sierra": "s",
    "tango": "t",
    "uniform": "u",
    "victor": "v",
    "whiskey": "w",
    "yankee": "y",
    "zulu": "z",
}

# Regular expressions allow partial words to be replaced.
WORD_REPLACE_REGEX = (
    ("^i'(.*)", "I'\\1"),
)
WORD_REPLACE_REGEX = tuple(
    (re.compile(match), replacement)
    for (match, replacement) in WORD_REPLACE_REGEX
)


# -----------------------------------------------------------------------------
# Main Processing Function

def pressKey(key):
    return (
        "xdotool",
        "key",
        key,
    )

def typeText(text):
    return (
        "xdotool",
        "type",
        "--clearmodifiers",
        # Use a value higher than twelve so the characters don't get skipped (tsk!).
        "--delay",
        "10",
        "--",
        text,
    )

def emacs_command(text):
    return [
        pressKey("alt+x"),
        typeText(text),
        pressKey("enter"),
    ]

def nerd_dictation_macro_process(command):
    args = command.split(" ")
    text_block = ""
    ends_in_stop = False
    if args[0] == "the" and len(args) > 1:
        args = args[1:]
    for i in range(1, len(args)):
        if args[i] == "stop" and i == len(args)-1:
            ends_in_stop = True
        else:
            text_block += args[i]
            if i != len(args)-1:
                text_block += " "
    if (args[0] == "expand"):
        return emacs_command("er/expand-region")
    if (args[0] == "snap"):
        return emacs_command("sp-beginning-of-sexp")
    if (args[0] == "beginning"):
        return [pressKey("control+a")]
    if (args[0] == "and" or args[0] == "end"):
        return [pressKey("control+e")]
    if (args[0] == "next"):
        return [pressKey("control+n")]
    if (args[0] == "previous"):
        return [pressKey("control+p")]
    if (args[0] == "lead"):
        return [pressKey("control+c")]
    if (args[0] == "cancel"):
        return [pressKey("control+g")]
    if (args[0] == "stop"):
        return [pressKey("enter")]
    if (args[0] == "copy"):
        return emacs_command("kill-ring-save")
    if (args[0] == "paste"):
        return emacs_command("yank")
    if (args[0] == "cut"):
        return emacs_command("cua-cut-region")
    if (args[0] == "undo"):
        return emacs_command("undo")
    if (args[0] == "toss"):
        return emacs_command("revert-buffer-no-confirm")
    if (args[0] == "chomp" or args[0] == "champ" or args[0] == "chump"):
        return [pressKey("alt+BackSpace")]
    if (args[0] == "back"):
        return [pressKey("BackSpace")]
    if (args[0] == "search"):
        emacs_cmd = [pressKey("control+s")]
        emacs_cmd.append(typeText(handle_text(text_block, " ")))
        if ends_in_stop:
            emacs_cmd.append(pressKey("enter"))
        return emacs_cmd
    if (args[0] == "backward" or args[0] == "backwards"):
        emacs_cmd = [pressKey("control+r")]
        emacs_cmd.append(typeText(handle_text(text_block, " ")))
        if ends_in_stop:
            emacs_cmd.append(pressKey("enter"))
        return emacs_cmd
    if (args[0] == "tab"):
        return [pressKey("Tab")]
    if (len(args) > 1):
        if (args[0] == "quote"):
            text_block = ""
            for word in args[1:]:
                text_block += word + " "
            return [
                typeText(text_block),
            ]
        if (args[0] == "test"):
            if (args[1] == "test"):
                return [("festival", "--tts", "/home/dvorak/.config/nerd-dictation/greeting.txt")]
        if (args[0] == "control"):
            if (args[1] == "you"):
                return [pressKey("control+u")]
        if (args[0] == "command"):
            return emacs_command(handle_text(text_block, "-"))
        if (args[0] == "save"):
            if (args[1] == "buffer"):
                return emacs_command("save-buffer")
        if (args[0] == "mark"):
            if (args[1] == "thing"):
                emacs_cmd = emacs_command("mark-whole-sexp")
                return emacs_cmd
            if (args[1] == "and"):
                return emacs_command("mark-sexp")
        if (args[0] == "kill"):
            if (args[1] == "line"):
                return emacs_command("kill-line")
        if (args[0] == "other"):
            if (args[1] == "window"):
                return emacs_command("other-window")
            if (args[1] == "buffer"):
                return emacs_command("other-buffer")
            if (args[1] == "client"):
                return [pressKey("alt+Tab")]
        if (args[0] == "scroll"):
            if (args[1] == "up"):
                return emacs_command("scroll-down")
            if (args[1] == "down"):
                return emacs_command("scroll-up")
        if (args[0] == "new"):
            if (args[1] == "line"):
                return emacs_command("newline-anywhere")
        if (args[0] == "dashed"):
            return [typeText(handle_text(text_block, "-"))]
        if (args[0] == "bashed"):
            return [typeText(handle_text(text_block, "-", caps="caps"))]
        if (args[0] == "close"):
            return [typeText(handle_text(text_block, ""))]
        if (args[0] == "dotted"):
            return [typeText(handle_text(text_block, "."))]
        if (args[0] == "snake"):
            return [typeText(handle_text(text_block, "_"))]
        if (args[0] == "bake"):
            return [typeText(handle_text(text_block, "_", caps="caps"))]
        if (args[0] == "caps"):
            return [typeText(handle_text(text_block, " ", caps="caps"))]
        if (args[0] == "camel"):
            return [typeText(handle_text(text_block, "", caps="camel"))]
        if (args[0] == "pascal"):
            return [typeText(handle_text(text_block, "", caps="pascal"))]
    return None

def nerd_dictation_process(text):
    return handle_text(text, " ");

def handle_text(text, seperator, caps=None):

    for match, replacement in TEXT_REPLACE_REGEX:
        text = match.sub(replacement, text)

    words = text.split(" ")

    for i, w in enumerate(words):
        w_init = w
        w_test = WORD_REPLACE.get(w)
        if w_test is not None:
            w = w_test

        if w_init == w:
            for match, replacement in WORD_REPLACE_REGEX:
                w_test = match.sub(replacement, w)
                if w_test != w:
                    w = w_test
                    break

        words[i] = w

    # Strip any words that were replaced with empty strings.
    words[:] = [w for w in words if w]

    for i in range(0, len(words)):
        if caps == "camel" and i != 0:
            words[i] = words[i].capitalize()
        if caps == "pascal":
            words[i] = words[i].capitalize()
        if caps == "caps":
            words[i] = words[i].upper()

    if seperator == "":
        text_block = ""
        for word in words:
            text_block += word
        return text_block

    return seperator.join(words)
