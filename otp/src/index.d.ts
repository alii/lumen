declare module 'arc:erlang' {
	export type Pid = import('./lib.d.ts').opaque<'pid'>;
	export function send<T>(pid: Pid, message: T): void;
	export function spawn<T>(fn: () => T): Pid;
	export function receive<T>(): T;
}
