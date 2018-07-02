# This file is a part of Julia. License is MIT: https://julialang.org/license

# Wrapping

function ansi_length(s)
    return length(replace(s, r"\e\[[0-9]+m" => "")) # TODO: use textwidth?
end

words(s) = split(s, " ")
lines(s) = split(s, "\n")
