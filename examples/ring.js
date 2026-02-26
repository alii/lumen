// Ring benchmark: pass a message around N processes M times.
// A classic Erlang concurrency demo.

var N = 100; // number of processes in the ring
var M = 10; // number of laps around the ring

// Build a ring of N processes. Each one receives a message and
// forwards it to the next process in the ring.
var first = Arc.self();
var prev = first;

var i = N;
while (i > 0) {
	var next = prev;
	prev = Arc.spawn(() => {
		while (true) {
			var msg = Arc.receive();
			if (msg.type === 'stop') {
				return;
			}
			Arc.send(next, { type: 'token', lap: msg.lap, hops: msg.hops + 1 });
		}
	});
	i = i - 1;
}

Arc.log('Ring of', N, 'processes created. Sending token for', M, 'laps...');

// Send the token into the ring
Arc.send(prev, { type: 'token', lap: 0, hops: 0 });

// Main process acts as the "first" node — receives and re-sends
var lap = 0;
while (lap < M) {
	var msg = Arc.receive(5000);
	if (msg === undefined) {
		Arc.log('Timed out waiting for token!');
		lap = M;
	} else {
		lap = msg.lap + 1;
		Arc.log('Lap', lap, 'complete -', msg.hops, 'hops');
		if (lap < M) {
			Arc.send(prev, { type: 'token', lap: lap, hops: 0 });
		}
	}
}

Arc.log('Done!', N, 'processes x', M, 'laps =', N * M, 'total message passes');
