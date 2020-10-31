set -eu

TIMESTAMP=$(date +%Y-%m)

NOTES_DIR=~/Notes
PLAINTEXT_NOTES=${NOTES_DIR}/notes.md
ENCRYPTED_NOTES=${NOTES_DIR}/notes.md.gpg
MONTHLY_ENCRYPTED_NOTES=${NOTES_DIR}/notes.${TIMESTAMP}.md.gpg

ntpull() {
        rclone copy cloud:Notes/notes.md.gpg ${NOTES_DIR}
        gpg --yes -q -o ${PLAINTEXT_NOTES} -d ${ENCRYPTED_NOTES}
        rm ${ENCRYPTED_NOTES}
}
