lint:
	hlint src tests
indent:
	./scripts/hindent.sh
indent-validate:
	./scripts/hindent-validate.sh
weeder:
	weeder --build .
