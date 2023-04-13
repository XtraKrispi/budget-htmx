target: watch_server watch_tailwind

watch_server:
	ghcid -c cabal repl -W -T main

watch_tailwind:
	npm run tw