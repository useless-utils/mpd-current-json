#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <mpd/client.h>

#define BUFFER_SIZE 10240

void escape_json_string(const char* input, char* output) {
  int input_len = strlen(input);
  int output_pos = 0;

  for (int i = 0; i < input_len; i++) {
    switch (input[i]) {
    case '\\':
      output[output_pos++] = '\\';
      output[output_pos++] = '\\';
      break;
    case '\n':
      output[output_pos++] = '\\';
      output[output_pos++] = 'n';
      break;
    case '\r':
      output[output_pos++] = '\\';
      output[output_pos++] = 'r';
      break;
    case '\t':
      output[output_pos++] = '\\';
      output[output_pos++] = 't';
      break;
    case '\f':
      output[output_pos++] = '\\';
      output[output_pos++] = 'f';
      break;
    case '"':
      output[output_pos++] = '\\';
      output[output_pos++] = '"';
      break;
    default:
      output[output_pos++] = input[i];
      break;
    }
  }

  output[output_pos] = '\0';
}

int main(int argc, char* argv[]) {
  int port = 6600;
  int verbose = 0;
  const char* host = "localhost";

  // Command line argument parsing
  int opt;
  while ((opt = getopt(argc, argv, "vp:")) != -1) {
    switch (opt) {
    case 'v':
      verbose = 1;
      break;
    case 'p':
      port = atoi(optarg);
      break;
    default:
      fprintf(stderr, "Usage: %s [-v] [-p port]\n", argv[0]);
      exit(1);
    }
  }

  // Connect to MPD server
  struct mpd_connection* connection = mpd_connection_new(host, port, 0);
  if (connection == NULL) {
    fprintf(stderr, "Failed to connect to MPD server.\n");
    exit(1);
  }

  // Send command and receive response
  struct mpd_status* status = mpd_run_status(connection);
  struct mpd_song* song = mpd_run_current_song(connection);

  if (song == NULL) {
    printf("{}\n");
    mpd_status_free(status);
    mpd_connection_free(connection);
    return 0;
  }

  char buffer[BUFFER_SIZE];
  int buffer_pos = 0;

  // open json object
  buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "{\n");

  // Parse song information
  // [[https://www.musicpd.org/doc/libmpdclient/tag_8h.html][libmpdclient: mpd/tag.h File Reference]]
  const char* title = mpd_song_get_tag(song, MPD_TAG_TITLE, 0);
  const char* artist = mpd_song_get_tag(song, MPD_TAG_ARTIST, 0);
  const char* album = mpd_song_get_tag(song, MPD_TAG_ALBUM, 0);
  const char* genre = mpd_song_get_tag(song, MPD_TAG_GENRE, 0);
  int duration = mpd_song_get_duration(song);
  const char* album_artist = mpd_song_get_tag(song, MPD_TAG_ALBUM_ARTIST, 0);

  if (title != NULL) {
    char escaped_title[BUFFER_SIZE];
    escape_json_string(title, escaped_title);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"title\":\"%s\",\n", escaped_title);
  }

  if (artist != NULL) {
    char escaped_artist[BUFFER_SIZE];
    escape_json_string(artist, escaped_artist);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"artist\":\"%s\",\n", escaped_artist);
  }

  if (album != NULL) {
    char escaped_album[BUFFER_SIZE];
    escape_json_string(album, escaped_album);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"album\":\"%s\",\n", escaped_album);
  }

  if (genre != NULL) {
    char escaped_genre[BUFFER_SIZE];
    escape_json_string(genre, escaped_genre);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"genre\":\"%s\",\n", escaped_genre);
  }

  if (album_artist != NULL) {
    char escaped_album_artist[BUFFER_SIZE];
    escape_json_string(album_artist, escaped_album_artist);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"albumartist\":\"%s\",\n", escaped_album_artist);
  }

  // last key without, trailing comma
  buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"duration\":%d\n", duration);
  // close json object
  buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "}\n");

  printf("%s", buffer);

  // Clean up
  mpd_song_free(song);
  mpd_status_free(status);
  mpd_connection_free(connection);

  return 0;
}
