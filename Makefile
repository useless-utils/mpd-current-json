CC = gcc
CFLAGS = -Wall -Wextra
LDFLAGS = -lmpdclient

TARGET = mpd-current-json
SRCS = mpd-current-json.c
OBJS = $(SRCS:.c=.o)

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET)
