# User configuration file typically located at `~/.config/nerd-dictation/nerd-dictation.py`
import re


# -----------------------------------------------------------------------------
# Replace Multiple Words

TEXT_REPLACE_REGEX = (
    ("\\b" "data type" "\\b", "data-type"),
    ("\\b" "copy on write" "\\b", "copy-on-write"),
    ("\\b" "key word" "\\b", "keyword"),
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
    "git def": "git diff",
    "git dif": "git diff",
    "space": " ",
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

def nerd_dictation_macro_process(command):
    args = command.split(" ")
    if (args[0] == "enter"):
        text_block = ""
        for word in args[1:]:
            text_block += word + " "
        return [(
            "xdotool",
            "key",
            "enter"
        )]
    if (len(args) > 1):
        if (args[0] == "command"):
            text_block = "";
            for i in range(1, len(args)):
                text_block += args[i];
                if i != len(args)-1:
                    if args[i] != "i":
                        text_block += "-"
                else:
                    text_block += '\n'

            return [
                (
                    "xdotool",
                    "key",
                    "alt+x",
                ),
                (
                    "xdotool",
                    "type",
                    "--clearmodifiers",
                    # Use a value higher than twelve so the characters don't get skipped (tsk!).
                        "--delay",
                    "10",
                    "--",
                    text_block,
                ),
            ]
        if (args[0] == "test"):
            if (args[1] == "test"):
                return [("festival", "--tts", "/home/dvorak/.config/nerd-dictation/greeting.txt")]
        if (args[0] == "quote"):
            text_block = "";
            for word in args[1:]:
                text_block += word + " "
            return [(
                "xdotool",
                "type",
                "--clearmodifiers",
                # Use a value higher than twelve so the characters don't get skipped (tsk!).
                "--delay",
                "10",
                "--",
                text_block,
            )]
        if (args[0] == "git"):
            text_block = "git ";
            for word in args[1:]:
                text_block += word + " "
            return [(
                "xdotool",
                "type",
                "--clearmodifiers",
                # Use a value higher than twelve so the characters don't get skipped (tsk!).
                "--delay",
                "10",
                "--",
                text_block,
            ),
            (
                "xdotool",
                "key",
                "enter"
            )]
    return None

def nerd_dictation_process(text):

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

    return " ".join(words)
