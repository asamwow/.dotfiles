# User configuration file typically located at `~/.config/nerd-dictation/nerd-dictation.py`
import re

# Usage:

# [<single commands>] [<text modifier>] <prose> [space,stop]
# [<single commands>] <multi-command>

# [<single commands>] : Chain any number of single commands
# [<text modifier>]   : Modify prose (caps, snake, camel...)
# <prose>             : Dictate any textual phrase
# <multi-command>     : Multi-word command, cannot be chained
# [space,stop]        : Finish input with space or enter key

# Emacs Commands that are dictated one to one
EMACS_COMMANDS = [
    "find file",
    "switch buffer",
    "query replace",
    "newline anywhere",
    "learn replacement",
    "bookmark jump",
    "bookmark set",
    "learn emacs command",
    "scroll up",
    "scroll down",
    "kill line",
    "other window",
    "other buffer",
    "save buffer",
]

# Replace Multiple Words

TEXT_REPLACE_REGEX = (
    ("\\b" "and of" "\\b", "end of"),
    ("\\b" "se[ae] [ae]nd" "\\b", "&&"),
    ("\\b" "se[ae] equal.?" "\\b", "=="),
    ("\\b" "se[ae] not" "\\b", "!="),
    ("\\b" "[kg][ae]t [hae]nd" "\\b", "git add"),
    ("\\b" "key word" "\\b", "keyword"),
    ("\\b" "for each" "\\b", "foreach"),
    ("\\b" "new line" "\\b", "newline"),
    ("\\b" "right boy" "\\b", ")"),
    ("\\b" "[ea]nd boy" "\\b", ")"),
    ("\\b" "right bracket" "\\b", "]"),
    ("\\b" "right curly" "\\b", "}"),
    ("\\b" "x ray" "\\b", "x"),
    ("\\b" "exclamation point" "\\b", "!"),
    ("\\b" "question mark" "\\b", "?"),
    ("\\b" "less than" "\\b", "<"),
    ("\\b" "greater than" "\\b", ">"),
    ("\\b" "d fun" "\\b", "defun"),
)
TEXT_REPLACE_REGEX = tuple(
    (re.compile(match), replacement)
    for (match, replacement) in TEXT_REPLACE_REGEX
)

# Replace Single Words

WORD_REPLACE = {
    "kit":"git",
    "arx":"args",
    "arcs":"args",
    "da":".",
    "dont":".",
    "kama":",",
    "cancer":"cancel",
    "get": "git",
    "the": "", # HACK
    "huh": "", # HACK
    "diary": "dired",
    "quad": "4",
    "quand": "4",
    "define": "defun",

    # symbols
    "dash": "-",
    "quotes": "\"",
    "boy": "(",
    "bracket": "[",
    "curly": "{",
    "slash": "/",
    "colon": ":",
    "colin": ":",
    "coin": ":",
    "cohen": ":",
    "apostrophe": "'",
    "comma": ",",
    "come": ",",
    "coma": ",",
    "karma": ",",
    "semi": ";",
    "dot": ".",
    "tilda": "~",
    "tick": "`",
    "equals": "=",
    "sequal": "==",
    "sequals": "==",
    "sequel": "==",
    "sequels": "==",
    "asterisk": "*",
    "carrot": "^",
    "amp": "&",
    "sand": "&&",
    "cn": "&&",
    "cnn": "&&",
    "san": "&&",
    "pipe": "|",
    "cr": "||",
    "at": "@",
    "hash": "#",
    "percent": "%",
    "underscore": "_",
    "backslash": "\\",

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

def process_single_word_macro(macro):
    if (macro == "expand" or macro == "enhance"):
        return emacs_command("er/expand-region")
    if (macro == "snap"):
        return emacs_command("sp-beginning-of-sexp")
    if (macro == "beginning"):
        return [pressKey("control+a")]
    if (macro == "and" or macro == "end"):
        return [pressKey("control+e")]
    if (macro == "next"):
        return [pressKey("control+n")]
    if (macro == "previous"):
        return [pressKey("control+p")]
    if (macro == "quit" or macro == "exit"):
        return [
            pressKey("control+g"),
            pressKey("control+g"),
            pressKey("control+g"),
        ]
    if (macro == "lead"):
        return [pressKey("control+c")]
    if (macro == "cancel"):
        return [pressKey("control+g")]
    if (macro == "stop"):
        return [pressKey("enter")]
    if (macro == "space"):
        return [typeText(" ")]
    if (macro == "copy"):
        return emacs_command("kill-ring-save")
    if (macro == "paste"):
        return emacs_command("yank")
    if (macro == "cut"):
        return emacs_command("cua-cut-region")
    if (macro == "undo" or macro == "fuck"):
        return emacs_command("undo")
    if (macro == "toss"):
        return emacs_command("revert-buffer-no-confirm")
    if (macro == "kill"):
        return emacs_command("kill-line")
    if (macro == "chomp" or macro == "champ" or macro == "chump"):
        return [pressKey("alt+f")]
    if (macro == "poop"):
        return [pressKey("alt+b")]
    if (macro == "slap"):
        return [pressKey("alt+BackSpace")]
    if (macro == "backpack"):
        return [pressKey("alt+BackSpace"),
                pressKey("alt+BackSpace")]
    if (macro == "back"):
        return [pressKey("BackSpace")]
    if (macro == "backward" or macro == "backwards"):
        return [pressKey("control+b")]
    if (macro == "forward"):
        return [pressKey("control+f")]
    if (macro == "com"):
        return [pressKey("alt+x")]
    if (macro == "tab" or macro == "indent"):
        return [pressKey("Tab")]
    if (macro == "search"):
        return [pressKey("control+s")]
    if (macro == "reverse"):
        return [pressKey("control+r")]
    if (macro == "repeat"):
        return [pressKey("control+u")]
    return None

def nerd_dictation_macro_process(command):
    args = command.split(" ")
    text_block = ""
    ends_in_stop = False
    ends_in_space = False
    if (args[0] == "the" or args[0] == "huh") and len(args) > 1:
        args = args[1:]
    for i in range(1, len(args)):
        if args[i] == "stop" and i == len(args)-1:
            ends_in_stop = True
        elif args[i] == "space" and i == len(args)-1:
            ends_in_space = True
        else:
            text_block += args[i]
            if i != len(args)-1:
                text_block += " "
    compound_macro = process_single_word_macro(args[0])
    if compound_macro != None:
        sub_macro = nerd_dictation_macro_process(text_block)
        if sub_macro == None:
            sub_macro = [typeText(handle_text(text_block, " "))]
        for cmd in sub_macro:
            compound_macro.append(cmd)
        if ends_in_stop:
            compound_macro.append(pressKey("enter"))
        if ends_in_space:
            compound_macro.append(typeText(" "))
        return compound_macro
    if (len(args) > 1):
        if (args[0] == "quote"):
            text_block = " ".join(args[1:])
            return [
                typeText(text_block),
            ]
        for emacs_function in EMACS_COMMANDS:
            if command == emacs_function:
                return emacs_command("-".join(args))
        if (args[0] == "command" or args[0] == "commands"):
            return emacs_command(handle_text(text_block, "-"))
        if (args[0] == "mark"):
            if args[1]  == "point":
                return emacs_command("cua-set-mark")
            if (args[1] == "thing"):
                return emacs_command("mark-whole-sexp")
            if (args[1] == "and"):
                return emacs_command("mark-sexp")
        if (args[0] == "other"):
            if (args[1] == "position"):
                return emacs_command("cua-exchange-point-and-mark")
            if (args[1] == "client"):
                return [pressKey("alt+Tab")]
        if (args[0] == "new"):
            if (args[1] == "line"):
                return emacs_command("newline-anywhere")
        if (args[0] == "dashed"):
            return [typeText(handle_text(text_block, "-"))]
        if (args[0] == "bashed"):
            return [typeText(handle_text(text_block, "-", caps="caps"))]
        if (args[0] == "close"):
            return [typeText(handle_text(text_block, ""))]
        if (args[0] == "fat"):
            return [typeText(handle_text(text_block, "", caps="caps"))]
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
        if ends_in_space:
            return [
                typeText(handle_text(args[0] + " " + text_block, " ")),
                typeText(" "),
            ]
        if ends_in_stop:
            return [
                typeText(handle_text(args[0] + " " + text_block, " ")),
                pressKey("enter"),
            ]
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
