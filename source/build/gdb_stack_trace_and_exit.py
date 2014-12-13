
def on_stop(event):
    print("Stopped, event = %s\n" % str(event))
    if isinstance(event, gdb.SignalEvent):
        gdb.execute("backtrace")
        gdb.execute("quit")

def on_exit(event):
    print("Exited, event = %s" % event)
    gdb.execute("quit")
    # if event.exit_code == 0:
    #     if raw_input("press x if you don't want to quit") != "x":
    #         gdb.execute("quit")

gdb.events.stop.connect(on_stop)
gdb.events.exited.connect(on_exit)

gdb.execute("set confirm off")
gdb.execute("run")

