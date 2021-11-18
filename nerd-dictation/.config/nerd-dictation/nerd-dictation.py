# User configuration file typically located at `~/.config/nerd-dictation/nerd-dictation.py`
import re


# -----------------------------------------------------------------------------
# Replace Multiple Words

TEXT_REPLACE_REGEX = (
    ("\\b" "key word" "\\b", "keyword"),
    ("\\b" "for each" "\\b", "foreach"),
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
    "space": " ",
    "dash": "-",
    "alpha": "a",
    "bravo": "b",
    "charlie": "c",
    "delta": "d",
    "echo": "e",
    "foxtro": "f",
    "golf": "g",
    "hotel": "h",
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

def nerd_dictation_macro_process(command):
    args = command.split(" ")
    if (args[0] == "enter"):
        text_block = ""
        for word in args[1:]:
            text_block += word + " "
        return [
            pressKey("enter"),
        ]
    if (len(args) > 1):
        if (args[0] == "command"):
            text_block = ""
            for i in range(1, len(args)):
                text_block += args[i]
                if i != len(args)-1:
                    text_block += "-"
            return [
                pressKey("alt+x"),
                typeText(text_block),
                pressKey("enter"),
            ]
        if (args[0] == "test"):
            if (args[1] == "test"):
                return [("festival", "--tts", "/home/dvorak/.config/nerd-dictation/greeting.txt")]
        if (args[0] == "quote"):
            text_block = ""
            for word in args[1:]:
                text_block += word + " "
            return [
                typeText(text_block),
            ]
        text_block = ""
        for i in range(1, len(args)):
            text_block += args[i]
            if i != len(args)-1:
                text_block += " "
        if (args[0] == "dashed"):
            return [
                typeText(handle_text(text_block, "-")),
            ]
        if (args[0] == "close"):
            return [
                typeText(handle_text(text_block, "")),
            ]
    return None



def nerd_dictation_process(text):
    return handle_text(text, " ");

def handle_text(text, seperator):

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

    if seperator == "":
        text_block = ""
        for word in words:
            text_block += word
        return text_block

    return seperator.join(words)
