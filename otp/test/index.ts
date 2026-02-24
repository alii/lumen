/// <reference path="../src/index.d.ts" />
declare function println(arg: unknown): void;

import { receive, send, spawn } from 'arc:erlang';

const pid = spawn(() => {
	const message = receive();
	println(message);
});

send(pid, 'Hello');
