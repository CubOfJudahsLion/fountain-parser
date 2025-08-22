#! /usr/bin/awk -f

## This script makes a few pattern-based transformation to add
## some LaTeX formatting to an ABNF grammar. It assumes the
## following packages installed and used in the master document: 
## color, hyperref, parskip, geometry and courier.


# Repeats the string str n times.
function repeat_str(n, str,    i, acc) {
    acc = "";
    for(i = 1; i <= n; ++i)
        acc = acc str;
    return acc;
}

BEGIN {
    print "{\\footnotesize\\texttt{{"; # start small monospaced text, please
    # tab conversion
    tab_size = 4;
    tab_replacement = repeat_str(" ", tab_size);
    # patterns
    rule_declaration_re = "^[A-Za-z][A-Za-z0-9-]*";
    hex_chars_re = "[xX][[:xdigit:]]+" "(" "(\\.[[:xdigit:]]+)+" "|" "-[[:xdigit:]]+" ")?";
    dec_chars_re = "[dD][[:digit:]]+" "(" "(\\.[[:digit:]]+)+" "|" "-[[:digit:]]+" ")?";
    bin_chars_re = "[bB][01]+" "(" "(\\.[01]+)+" "|" "-[01]+" ")?";
    quoted_string_re = "\"[^\"]*\"";
    string_re = "%(" hex_chars_re "|" dec_chars_re "|" bin_chars_re ")" "|" quoted_string_re;
}

# We might separate comments from other text to prevent further processing.
# Start with a null string every line because we might find no comments.
{ comment = ""; }

# Blank lines are just printed and further processing is skipped.
/^[[:blank:]]*$/ {
    print "\\\\";
    next;
}

# Tabs are replaced by the defined number of spaces.
/\t/ { gsub(/\t/, nchars(" ", tab_replacement)); }

# Handle backslash literals before we add LaTeX commands.
/\\/ { gsub(/\\/, "\\textbackslash{}"); }

# Comments are grayed out. The comment is separated to avoid formatting for now.
# It will be joined back later.
/;/ {
    comment = $0;
    comment = gensub(/^[^;]*(;.*)$/, "\\\\textcolor{gray}{\\1}", 1, comment);
    $0 = gensub(/^([^;]*);.*$/, "\\1", 1);
}

# Rule names in declarations are shown in boldface.
$0 ~ rule_declaration_re { sub(rule_declaration_re, "\\textbf{&}"); }

# Strings are colored red.
$0 ~ string_re { gsub(string_re, "\\textcolor{red}{&}"); }

# Replace all LaTeX special characters with equivalent literals.
{
    $0 = $0 comment; # join the comment back in
    gsub(/([_#%&$])/, "\\\\&{}");
    gsub(/~/, "\\textasciitilde{}");
    gsub(/\^/, "\\textasciicircum{}");
    gsub(/</, "\\textless{}");
    gsub(/>/, "\\textgreater{}");
}

# Converts all sequences of multiple spaces in a string
# into an \mbox of non-breaking spaces.
function mbox_spaces(str,    parts, p, acc) {
    patsplit(str, parts, / +|[^ ]+/);
    acc = "";
    for(p in parts) {
        if(parts[p] ~ / {2,}/) {
            gsub(/ /, "~", parts[p]);
            parts[p] = "\\mbox{" parts[p] "}";
        }
        acc = acc parts[p];
    }
    return acc;
}

# Now that it's all spaces and that we've escaped literal tildes:
# if they're two or more spaces, they're \mboxed and replaced by
# non-breaking ("~") to preserve spacing.
/ {2,}/ { $0 = mbox_spaces($0); }

# Add explicit line terminators.
{ print $0 "\\\\" }

END {
    print "}}}"; # end small monospaed text
}
