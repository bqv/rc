def import_fish_history():
    import os
    FISH_HIST = "~/.local/share/fish/fish_history"
    fish_history = os.path.expanduser(FISH_HIST)

    from collections import OrderedDict
    cmds = OrderedDict()
    with open(fish_history) as fr:
        command = None
        timestamp = None
        for line in fr.read().splitlines():
            if line.startswith("-"):
                cmds[command] = timestamp
                _, cmd = line.split("cmd:")
                command = cmd.strip()
            elif "when:" in line:
                _, ts = line.split("when:")
                timestamp = int(ts.strip())
        cmds[command] = timestamp
	print(len(cmds))

    from xonsh.history.main import construct_history
    hist = construct_history(
        gc=False,
        # exlictily set buffersize to prevent slowing down to…
        # …a crawl because of excessive flushing
        buffersize=len(cmds)
    )
 
    for line in cmds.items():
        command = line[0]
        timestamp = line[1]
	if command is not None:
            command = command.replace(r'\\n', '\n')
            hist.append({'inp': command, 'rtn': 0, 'ts': (timestamp, timestamp+1)})
    print(hist.info()['filename'])
    hist.flush()
