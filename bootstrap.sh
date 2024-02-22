# Generate a new SSH key and add it to GitHub
# Clone the dotfiles

if ! command -v git &> /dev/null; then
	sudo dnf install -q git
fi

if ! command -v gh &> /dev/null; then
	sudo dnf install -q gh
  # NOTE: On Arch this package is called github-cli
fi

if [ ! -e ~/.ssh/id_ed25519.pub ]; then
	echo "Generating SSH key"
	exit
	ssh-keygen -t ed25519 -C "simonfv@gmail.com"
	eval "$(ssh-agent -s)"
	ssh-add ~/.ssh/id_ed25519

	echo "Login to GitHub"
	gh auth login

	# gh auth refresh -h github.com -s admin:ssh_signing_key

	echo "Add SSH key to GitHub"
	gh ssh-key add ~/.ssh/id_ed25519.pub --type signing
fi

if [ ! -e ~/config ]; then
	gh repo clone paldepind/dotfiles ~/config
fi
