function __yx_ps1()
{
    if [[ ! -v 'YX_VERSION' ]]; then
        return
    fi

    if [[ -v 'YX_ENVIRONMENT' && "${YX_ENVIRONMENT}" != '_default' ]]; then
        local yxEnv=":${YX_ENVIRONMENT}"
    fi

    printf "yx:%s%s\n" "${YX_PROJECT}" "${yxEnv}"
}

function __yx_ps1_pretty()
{
    if [[ ! -v 'YX_VERSION' ]]; then
        return
    fi

    printf " (%s)\n" "$(__yx_ps1)"
}
