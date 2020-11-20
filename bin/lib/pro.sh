pro() {
    if echo $@ | grep -q -E "${PROJECTS_ROOT}/([^/]+)/?(.*)?"; then
        echo $@ | sed -E "s|${PROJECTS_ROOT}/([^/]+)/?(.*)?|\1\t\2|"
        exit 0
    else
        exit -1
    fi
}
