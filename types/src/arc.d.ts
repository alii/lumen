declare module 'arc' {
	export type Pid = import('./lib.js').opaque<'pid'>;
	export function send<T>(pid: Pid, message: T): void;
	export function spawn<T>(fn: () => T): Pid;
	export function receive<T>(): T;
	export function self(): Pid;
	export function log(...args: unknown[]): void;
	export function sleep(ms: number): void;

	export function peek<T>(
		promise: Promise<T>,
	): { type: 'pending' } | { type: 'resolved'; value: T } | { type: 'rejected'; reason: unknown };
}
