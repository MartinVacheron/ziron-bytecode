# ANSI escape codes
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
BLUE = "\033[34m"
RESET = "\033[0m"

def clr_str(msg: str, clr: str) -> str:
    return f"{clr}{msg}{RESET}"