#Assumptions:

1. There is no single source of synchronized time, each agent has its own source.
According to the requirement, no 2 messages can have the same time. Since this cannot be guaranteed, if 2 messages have the same time then the one with the greatest value is kept.

1. Each agent creates (and sends) messages.

1. To make the whole process repeatable (to some extend), messages (times and values) are generated from the deterministic RNG.

1. Each agent manages its own submission and grace periods. When an agent enters the grace period, the others are not notified (idem when an agent terminates). Code that forwards these events is present but commented out (uncomment it if you want to see what happens).

1. Agents do not all start at the same time.

1. Agents can be stopped and restarted at will.


#Algorithm.

Each agent maintains a payload (see *Payload* and *WeightedSum* data types) which is a time ordered list of all the messages it has received so far.

When an agent creates a new message, it appends it to its current payload and sends that new payload to all other agents. Thus, all information known by an agent is always shared.

When an agent receives a payload, it compares it with its own payload and either keeps one of them or merge the two payloads.
The comparison process tries to maximize the weighed sum of the choosen payload. So, that choosen payload could be considered as the "best" one.

* If the agent's payload is kept, it sends it back to the sender. It does not send that payload to the other nodes because it was already sent, previously.

* If the sender's payload is kept, nothing happens because the sender has already sent that payload (thus, the best one in this case) to all other agents.

* If both payloads are merged then the new payload is sent to all agents.

The payload size increases continuously. To limit the payload size, a compaction can be made based on the number of messages carried by the payload at a given time (see the functions *shoudCompactPayload* and *compactPayload*).

Compacting payloads may lead to the rejection of some messages (see the function *addMsg* ). A message whose time is before the *timeLastMsg* of the weighted sum is rejected. Indeed, information about time (thus, about message indexes) for the messages that have already been compacted is lost.


#Usage:

Usage: iohk-exe [-h|--host ARG] [-p|--port ARG] [--send-for ARG]
                [--wait-for ARG] [--with-seed ARG] [--nb-msgs ARG]

Available options:

| Option          |      Meaning                                                 |
|-----------------|--------------------------------------------------------------|
| -h,--host ARG   |  Host address                                                |
| -p,--port ARG   |  Port                                                        |
| --send-for ARG  |  Submission period in seconds                                |
| --wait-for ARG  |  Grace period in seconds                                     |
| --with-seed ARG |  Seed                                                        |
| --nb-msgs ARG   |  Number of messages to generate during the submission period |

#Configuration:

*defNodes* in Main.hs is a list of nodes (host + port) together with a (per agent) seed and number of messages to generate.

These parameters can overridden with command line options


