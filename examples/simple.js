const pid = Arc.spawn(() => {
	const message = Arc.receive();
	Arc.log(message);
	Arc.log(`${Arc.self()}: Hello from child`);
});

Arc.send(pid, `${Arc.self()}: Hello from main`);
