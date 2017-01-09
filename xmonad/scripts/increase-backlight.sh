#/bin/bash

# Increase LCD brightness by 10.
# Assumes that lcd-brightness command is on $PATH.
lcd-brightness $(expr $(lcd-brightness) + 100)
