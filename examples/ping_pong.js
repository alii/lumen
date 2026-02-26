// Classic Erlang ping-pong between two processes.

var pong_pid = Arc.spawn(() => {
	while (true) {
		var msg = Arc.receive();
		Arc.log('pong received:', msg.text, 'from', msg.from);
		Arc.send(msg.from, { text: 'pong', count: msg.count + 1, from: Arc.self() });
		if (msg.count >= 5) {
			return;
		}
	}
});

// Ping loop in the main process
var count = 0;
Arc.send(pong_pid, { text: 'ping', count: count, from: Arc.self() });

while (count < 5) {
	var reply = Arc.receive(2000);
	if (reply === undefined) {
		Arc.log('Timed out!');
		count = 999;
	} else {
		Arc.log('ping received:', reply.text, 'count:', reply.count);
		count = reply.count;
		if (count < 5) {
			Arc.send(pong_pid, { text: 'ping', count: count, from: Arc.self() });
		}
	}
}

Arc.log('Done! Final count:', count);
