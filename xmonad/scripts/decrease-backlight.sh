#/bin/bash

# Decrease LCD backlight value.
# Assumes that lcd-brightness command is on $PATH.
lcd-brightness $(expr $(lcd-brightness) - 100)
