/^@doc|^==/ {
    close_markdown();
}
in_markdown {
    gsub(/\{@/,"{\\@")
    print | markdown;
}
!in_markdown {
    print;
}
!in_markdown && /^ *$/ {
    open_markdown();
}

function close_markdown() {
    close(markdown);
    in_markdown = 0;
}

function open_markdown() {
    markdown = markdown_command;
    in_markdown = 1;
}
