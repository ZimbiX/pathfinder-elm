id=g

reset() {
  curl -sSf 'www.zimbico.net/pathfinder-elm-backend/db-setup.php?reset'
}

move() {
  local direction="$1"
  curl -sSf -i \
    www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php \
    -d id="$id" \
    -d event='{"name": "'"$direction"'", "data": {}}' \
    -d version=$((++i))
}

alias w='move MoveUp'
alias a='move MoveLeft'
alias s='move MoveDown'
alias d='move MoveRight'
alias r='reset'
