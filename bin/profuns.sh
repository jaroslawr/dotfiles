function pro() {
    projects_root="${HOME}/Projects"
    if echo "$@" | grep -q -E "${projects_root}/([^/]+)/?(.*)?"; then
        echo "$@" | sed -E "s|${projects_root}/([^/]+)/?(.*)?|\1\t\2|"
        exit 0
    else
        exit -1
    fi
}
