README ASGN3
Karl Munson
CSE130

Instructions: To run and test loadbalancer use the command make to parse the included Makefile.
This will generate an executable called loadbalancer which can then be called with ./loadbalancer <first port number> <second port number> ... Optionally loadbalancer can be called with -N <thread count> or -R <requests>
for a different thread count or to health check after R requests. Optional arguements can be called in any order. The first port number specified is the port load balancer is located at.
After running the executable loadbalancer will be up and can recieve http requests over the first port.
Httpservers should be hosted at the other specified port numbers other than the first port number.
Makefile also includes clean which deletes .o files.

Files included: DESIGN.pdf README.md WRITEUP.pdf loadbalancer.c
