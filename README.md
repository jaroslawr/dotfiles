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


## Manual configuration

`.ssh/config`

```
AddKeysToAgent yes
```