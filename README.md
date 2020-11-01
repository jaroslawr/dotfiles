## Usage

Clone:

```
git clone --recurse-submodules git@github.com:jaroslawr/dotfiles.git
```

Install:

```
./install.sh
```

Sync submodules:

```
git submodule init
git submodule update --recursive
```

Update submodules to newest remote revisions:

```
git submodule update --recursive --remote
```

Remove submodule:

```
git submodule deinit <path>
git rm <path>
git commit -m "Removed <submodule>."
rm -rf .git/modules/<path>
```

## Manual configuration

`.ssh/config`

```
AddKeysToAgent yes
```
