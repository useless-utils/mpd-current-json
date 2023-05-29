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

// Helper function to add a tag to the JSON output
void add_tag(char* buffer, size_t* buffer_pos, const char* tag_name, const char* tag_value) {
  if (tag_value != NULL) {
    char escaped_value[BUFFER_SIZE];
    escape_json_string(tag_value, escaped_value);

    // Check if tag_value is a number
    char* endptr;
    long long_value = strtol(tag_value, &endptr, 10);
    if (*endptr == '\0') {
      // tag_value is an int
      *buffer_pos += snprintf(buffer + *buffer_pos, BUFFER_SIZE - *buffer_pos, "\"%s\":%ld,", tag_name, long_value);
    } else {
      // tag_value is a string
      *buffer_pos += snprintf(buffer + *buffer_pos, BUFFER_SIZE - *buffer_pos, "\"%s\":\"%s\",", tag_name, escaped_value);
    }
  }
}


int main(int argc, char* argv[]) {
  int port = 6600;
  // int verbose = 0;
  // const char* host = "localhost";
  char* host = "localhost";

  // Command line argument parsing
  int opt;
  int option_index = 0;
  char* host_arg;

  // Define the long options array
  static struct option long_options[] = {
    // {"verbose", no_argument, 0, 'v'},
    {"port", required_argument, 0, 'p'},
    {"host", required_argument, 0, 'h'},
    {0, 0, 0, 0}
  };

  while ((opt = getopt_long(argc, argv, "vp:", long_options, &option_index)) != -1) {
    switch (opt) {
    /* case 'v': */
    /*   verbose = 1; */
    /*   break; */
    case 'h':
      host_arg = (char*)malloc(strlen(optarg) + 1);
      if (host_arg != NULL) {
        strcpy(host_arg, host);
      }
      else {
        fprintf(stderr, "ERROR: Memory allocation for host failed!\n");
        exit(EXIT_FAILURE);
      }
      break;
    case 'p':
      port = atoi(optarg);
      break;
    default:
      fprintf(stderr, "Usage: %s [-h,--host hostadress] [--port number]\n", argv[0]);
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
  size_t buffer_pos = 0;

  // open json object
  buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "{");

  // Parse song information
  // [[https://www.musicpd.org/doc/libmpdclient/tag_8h.html][libmpdclient: mpd/tag.h File Reference]]
  add_tag(buffer, &buffer_pos, "artist", mpd_song_get_tag(song, MPD_TAG_ARTIST, 0));
  add_tag(buffer, &buffer_pos, "album", mpd_song_get_tag(song, MPD_TAG_ALBUM, 0));
  add_tag(buffer, &buffer_pos, "album_artist", mpd_song_get_tag(song, MPD_TAG_ALBUM_ARTIST, 0));
  add_tag(buffer, &buffer_pos, "title", mpd_song_get_tag(song, MPD_TAG_TITLE, 0));
  add_tag(buffer, &buffer_pos, "track", mpd_song_get_tag(song, MPD_TAG_TRACK, 0));
  add_tag(buffer, &buffer_pos, "name", mpd_song_get_tag(song, MPD_TAG_NAME, 0));
  add_tag(buffer, &buffer_pos, "genre", mpd_song_get_tag(song, MPD_TAG_GENRE, 0));
  add_tag(buffer, &buffer_pos, "date", mpd_song_get_tag(song, MPD_TAG_DATE, 0));
  add_tag(buffer, &buffer_pos, "composer", mpd_song_get_tag(song, MPD_TAG_COMPOSER, 0));
  add_tag(buffer, &buffer_pos, "performer", mpd_song_get_tag(song, MPD_TAG_PERFORMER, 0));
  add_tag(buffer, &buffer_pos, "comment", mpd_song_get_tag(song, MPD_TAG_COMMENT, 0));
  add_tag(buffer, &buffer_pos, "disc", mpd_song_get_tag(song, MPD_TAG_DISC, 0));
  add_tag(buffer, &buffer_pos, "musicbrainz_artist_id", mpd_song_get_tag(song, MPD_TAG_MUSICBRAINZ_ARTISTID, 0));
  add_tag(buffer, &buffer_pos, "musicbrainz_album_id", mpd_song_get_tag(song, MPD_TAG_MUSICBRAINZ_ALBUMID, 0));
  add_tag(buffer, &buffer_pos, "musicbrainz_album_artist_id", mpd_song_get_tag(song, MPD_TAG_MUSICBRAINZ_ALBUMARTISTID, 0));
  add_tag(buffer, &buffer_pos, "musicbrainz_track_id", mpd_song_get_tag(song, MPD_TAG_MUSICBRAINZ_TRACKID, 0));
  add_tag(buffer, &buffer_pos, "musicbrainz_releasetrack_id", mpd_song_get_tag(song, MPD_TAG_MUSICBRAINZ_RELEASETRACKID, 0));
  add_tag(buffer, &buffer_pos, "original_date", mpd_song_get_tag(song, MPD_TAG_ORIGINAL_DATE, 0));
  add_tag(buffer, &buffer_pos, "artist_sort", mpd_song_get_tag(song, MPD_TAG_ARTIST_SORT, 0));
  add_tag(buffer, &buffer_pos, "album_artist_sort", mpd_song_get_tag(song, MPD_TAG_ALBUM_ARTIST_SORT, 0));
  add_tag(buffer, &buffer_pos, "album_sort", mpd_song_get_tag(song, MPD_TAG_ALBUM_SORT, 0));
  add_tag(buffer, &buffer_pos, "label", mpd_song_get_tag(song, MPD_TAG_LABEL, 0));
  add_tag(buffer, &buffer_pos, "musicbrainz_work_id", mpd_song_get_tag(song, MPD_TAG_MUSICBRAINZ_WORKID, 0));
  add_tag(buffer, &buffer_pos, "grouping", mpd_song_get_tag(song, MPD_TAG_GROUPING, 0));
  add_tag(buffer, &buffer_pos, "work", mpd_song_get_tag(song, MPD_TAG_WORK, 0));
  add_tag(buffer, &buffer_pos, "conductor", mpd_song_get_tag(song, MPD_TAG_CONDUCTOR, 0));
  add_tag(buffer, &buffer_pos, "count", mpd_song_get_tag(song, MPD_TAG_COUNT, 0));

  // Get current status
  if (status != NULL) {
    double elapsed = mpd_status_get_elapsed_ms(status) / 1000.0;
    double duration = mpd_song_get_duration_ms(song) / 1000.0;

    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"elapsed\":%.3f,", elapsed);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"duration\":%.3f,", duration);
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"position\":%u,", mpd_status_get_song_pos(status));
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"playlist_length\":%u,", mpd_status_get_queue_length(status));
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"repeat\":%s,", mpd_status_get_repeat(status) ? "true" : "false");
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"random\":%s,", mpd_status_get_random(status) ? "true" : "false");
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"single\":%s,", mpd_status_get_single(status) ? "true" : "false");
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"consume\":%s,", mpd_status_get_consume(status) ? "true" : "false");
    buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"bitrate\":%u,", mpd_status_get_kbit_rate(status));
  }

  // Convert the playback state to its string representation
  const char* state_string;
  switch (mpd_status_get_state(status)) {
  case MPD_STATE_PLAY:
    state_string = "play";
    break;
  case MPD_STATE_PAUSE:
    state_string = "pause";
    break;
  case MPD_STATE_STOP:
    state_string = "stop";
    break;
  default:
    state_string = "unknown";
    break;
  }

  buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "\"state\":\"%s\",", state_string);

  // Remove the trailing comma if any
  if (buffer[buffer_pos - 1] == ',') {
    buffer_pos--;
  }
  buffer_pos += snprintf(buffer + buffer_pos, BUFFER_SIZE - buffer_pos, "}\n");

  printf("%s", buffer);

  // Clean up
  mpd_song_free(song);
  mpd_status_free(status);
  mpd_connection_free(connection);
  free(host_arg);

  return 0;
}
