reset() {
  curl -sSf 'www.zimbico.net/pathfinder-elm-backend/db-setup.php?reset'
  i=0
}

move() {
  local direction="$1"
  curl -sSf -i \
    www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php \
    -d id="${game_id:-x}" \
    -d event='{"name": "'"$direction"'", "data": {}}' \
    -d version=$((++i))
}

all() {
  curl -sS "http://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php?id=${game_id:-x}&after=0&all" -v | jq .
}

alias w='move MoveUp'
alias a='move MoveLeft'
alias s='move MoveDown'
alias d='move MoveRight'
alias r='reset'
