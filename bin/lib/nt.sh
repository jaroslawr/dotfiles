set -eu

TIMESTAMP=$(date +%Y-%m)

NOTES_DIR=~/Notes
PLAINTEXT_NOTES=${NOTES_DIR}/notes.md
ENCRYPTED_NOTES=${NOTES_DIR}/notes.md.gpg
MONTHLY_ENCRYPTED_NOTES=${NOTES_DIR}/notes.${TIMESTAMP}.md.gpg

ntpull() {
        rclone copy cloud:Notes/notes.md.gpg ${NOTES_DIR}
        gpg -q --yes -o ${PLAINTEXT_NOTES} -d ${ENCRYPTED_NOTES}
        rm ${ENCRYPTED_NOTES}
}

cleanup() {
        if [[ -f ${ENCRYPTED_NOTES} ]]; then
                rm ${ENCRYPTED_NOTES}
        fi

        if [[ -f ${MONTHLY_ENCRYPTED_NOTES} ]]; then
                rm ${MONTHLY_ENCRYPTED_NOTES}
        fi
}

trap cleanup EXIT
