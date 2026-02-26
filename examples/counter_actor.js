// A counter actor — a stateful process that responds to messages.
// This is the classic Erlang/OTP actor pattern, in JavaScript.

var parent = Arc.self();

var counter = Arc.spawn(() => {
	var count = 0;
	while (true) {
		var msg = Arc.receive();
		if (msg.type === 'inc') {
			count = count + msg.n;
			Arc.log('counter: incremented by', msg.n, '-> now', count);
		}
		if (msg.type === 'dec') {
			count = count - msg.n;
			Arc.log('counter: decremented by', msg.n, '-> now', count);
		}
		if (msg.type === 'get') {
			Arc.send(msg.from, { type: 'value', count: count });
		}
		if (msg.type === 'stop') {
			Arc.log('counter: stopping with final value', count);
			Arc.send(msg.from, { type: 'stopped', count: count });
			return;
		}
	}
});

Arc.send(counter, { type: 'inc', n: 10 });
Arc.send(counter, { type: 'inc', n: 5 });
Arc.send(counter, { type: 'dec', n: 3 });
Arc.send(counter, { type: 'inc', n: 100 });
Arc.send(counter, { type: 'get', from: parent });

var result = Arc.receive(1000);
Arc.log('Main got counter value:', result.count);

Arc.send(counter, { type: 'stop', from: parent });
var stopped = Arc.receive(1000);
Arc.log('Counter stopped with:', stopped.count);
