/*
Tugas 1 Pemodelan Simulasi
Jon Felix Germinian/13518025
Filbert Wijaya/13518077
*/

/* External definitions for job-shop model. */

#include "simlib.h" /* Required for use of simlib.c. */
#include <ctype.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#define EVENT_ARRIVAL 1 /* Event type for arrival of a job to the system. */
#define EVENT_DEPARTURE 2 /* Event type for departure of a job from a particular station. */
#define EVENT_END_SIMULATION 3 /* Event type for end of the simulation. */
#define STREAM_INTERARRIVAL 1  /* Random-number stream for interarrivals. */
#define STREAM_JOB_TYPE 2      /* Random-number stream for job types. */
#define STREAM_SERVICE 3       /* Random-number stream for service times. */
#define MAX_NUM_STATIONS 50     /* Maximum number of stations. */
#define MAX_NUM_JOB_TYPES 50    /* Maximum number of job types. */

/* Variables Declaration */

// double mean_interarrival, length_simulation;

// int num_stations = 3;
// int num_job_types = 2;
// int *num_machines;
// int num_tasks[3] = {0, 2, 2};
// int route[3][3] = {{0, 0, 0}, {0, 1, 3}, {0, 2, 3}};

// int i, j, num_machines_busy[4], job_type, task;
// double prob_distrib_job_type[3] = {0, 0.5, 1.0};

// double mean_service[3][4];

int num_stations, num_job_types, i, j, num_machines[MAX_NUM_STATIONS + 1],
  num_tasks[MAX_NUM_JOB_TYPES + 1],
  route[MAX_NUM_JOB_TYPES + 1][MAX_NUM_STATIONS + 1], num_machines_busy[MAX_NUM_STATIONS + 1], job_type, task;
double mean_interarrival, length_simulation, prob_distrib_job_type[26],
  mean_service[MAX_NUM_JOB_TYPES + 1][MAX_NUM_STATIONS + 1];
FILE *infile, *outfile;


FILE *infile, *outfile;

void arrive(int new_job) /* Function to serve as both an arrival event of a job
                            to the system, as well as the non-event of a job's
                            arriving to a subsequent station along its
                            route. */
{
  int station;

  /* If this is a new arrival to the system, generate the time of the next
     arrival and determine the job type and task number of the arriving
     job. */

  if (new_job == 1) {
    event_schedule(sim_time + expon(mean_interarrival, STREAM_INTERARRIVAL),
                   EVENT_ARRIVAL);
    job_type = random_integer(prob_distrib_job_type, STREAM_JOB_TYPE);
    task = 1;
  }

  /* Determine the station from the route matrix. */

  station = route[job_type][task];

  /* Check to see whether all machines in this station are busy. */

  if (num_machines_busy[station] == num_machines[station]) {
    /* All machines in this station are busy, so place the arriving job at
       the end of the appropriate queue. Note that the following data are
       stored in the record for each job:
       1. Time of arrival to this station.
       2. Job type.
       3. Current task number. */

    transfer[1] = sim_time;
    transfer[2] = job_type;
    transfer[3] = task;
    list_file(LAST, station);
  }

  else {
    /* A machine in this station is idle, so start service on the arriving
       job (which has a delay of zero). */

    sampst(0.0, station);                 /* For station. */
    sampst(0.0, num_stations + job_type); /* For job type. */
    ++num_machines_busy[station];
    timest((double)num_machines_busy[station], station);

    /* Schedule a service completion.  Note defining attributes beyond the
       first two for the event record before invoking event_schedule. */

    transfer[3] = job_type;
    transfer[4] = task;
    event_schedule(sim_time +
                       erlang(2, mean_service[job_type][task], STREAM_SERVICE),
                   EVENT_DEPARTURE);
  }
}

void depart(void) /* Event function for departure of a job from a particular
                     station. */
{
  int station, job_type_queue, task_queue;

  /* Determine the station from which the job is departing. */

  job_type = transfer[3];
  task = transfer[4];
  station = route[job_type][task];

  /* Check to see whether the queue for this station is empty. */

  if (list_size[station] == 0) {
    /* The queue for this station is empty, so make a machine in this
       station idle. */

    --num_machines_busy[station];
    timest((double)num_machines_busy[station], station);
  }

  else {
    /* The queue is nonempty, so start service on first job in queue. */

    list_remove(FIRST, station);

    /* Tally this delay for this station. */

    sampst(sim_time - transfer[1], station);

    /* Tally this same delay for this job type. */

    job_type_queue = transfer[2];
    task_queue = transfer[3];
    sampst(sim_time - transfer[1], num_stations + job_type_queue);

    /* Schedule end of service for this job at this station.  Note defining
       attributes beyond the first two for the event record before invoking
       event_schedule. */

    transfer[3] = job_type_queue;
    transfer[4] = task_queue;
    event_schedule(sim_time + erlang(2,
                                     mean_service[job_type_queue][task_queue],
                                     STREAM_SERVICE),
                   EVENT_DEPARTURE);
  }

  /* If the current departing job has one or more tasks yet to be done, send
     the job to the next station on its route. */

  if (task < num_tasks[job_type]) {
    ++task;
    arrive(2);
  }
}

void report(void) /* Report generator function. */
{
  int i;
  double overall_avg_job_tot_delay, avg_job_tot_delay, sum_probs;

  /* Compute the average total delay in queue for each job type and the
     overall average job total delay. */

  fprintf(outfile, "\n\n\n\nJob type     Average total delay in queue");
  overall_avg_job_tot_delay = 0.0;
  sum_probs = 0.0;
  for (i = 1; i <= num_job_types; ++i) {
    avg_job_tot_delay = sampst(0.0, -(num_stations + i)) * num_tasks[i];
    fprintf(outfile, "\n\n%4d%27.3f", i, avg_job_tot_delay);
    overall_avg_job_tot_delay +=
        (prob_distrib_job_type[i] - sum_probs) * avg_job_tot_delay;
    sum_probs = prob_distrib_job_type[i];
  }
  fprintf(outfile, "\n\nOverall average job total delay =%10.3f\n",
          overall_avg_job_tot_delay);

  /* Compute the average number in queue, the average utilization, and the
     average delay in queue for each station. */

  fprintf(outfile,
          "\n\n\n Work      Average number      Average       Average delay");
  fprintf(outfile,
          "\nstation       in queue       utilization        in queue");
  for (j = 1; j <= num_stations; ++j)
    fprintf(outfile, "\n\n%4d%17.3f%17.3f%17.3f", j, filest(j),
            timest(0.0, -j) / num_machines[j], sampst(0.0, -j));
}

int main(int argc, char **argv) /* Main function. */
{
  /* Open input and output files. */

  printf ("Tugas 1 IF4021 Modsim\n\n");


  int opt;
  char* input_file;
  char* output_file;

  while ((opt = getopt (argc, argv, "i:o:")) != -1){
  switch (opt)
    {
    case 'i':
      infile = fopen(optarg, "r");
      input_file = optarg;
      break;
    case 'o':
      outfile = fopen(optarg, "w+");
      output_file = optarg;
      break;
    case '?':
      if (isprint (optopt))
        fprintf (stderr, "Unknown option `-%c'.\n", optopt);
      else
        fprintf (stderr,
                  "Unknown option character `\\x%x'.\n",
                  optopt);
      return 1;
    default:
      abort();
    }
  }
  // infile = fopen("demo1.in", "r");
  // outfile = fopen("demo1.out", "w+");

  
  /* Read input parameters. */

  fscanf (infile, "%d %d %lg %lg", &num_stations, &num_job_types, &mean_interarrival, &length_simulation);
  for (j = 1; j <= num_stations; ++j){
    fscanf (infile, "%d", &num_machines[j]);
  }
  for (i = 1; i <= num_job_types; ++i){
    fscanf (infile, "%d", &num_tasks[i]);
  }
  for (i = 1; i <= num_job_types; ++i)
    {
      for (j = 1; j <= num_tasks[i]; ++j){
	      fscanf (infile, "%d", &route[i][j]);
      }
      for (j = 1; j <= num_tasks[i]; ++j){
	      fscanf (infile, "%lg", &mean_service[i][j]);
      }
    }
  for (i = 1; i <= num_job_types; ++i)
    fscanf (infile, "%lg", &prob_distrib_job_type[i]);

  /* Write report heading and input parameters. */

  fprintf(outfile, "Tugas 1 Modsim \n\n");
  fprintf(outfile, "Number of work stations%21d\n\n", num_stations);
  fprintf(outfile, "Number of machines in each station     ");
  for (j = 1; j <= num_stations; ++j)
    fprintf(outfile, "%5d", num_machines[j]);
  fprintf(outfile, "\n\nNumber of job types%25d\n\n", num_job_types);
  fprintf(outfile, "Number of tasks for each job type      ");
  for (i = 1; i <= num_job_types; ++i)
    fprintf(outfile, "%5d", num_tasks[i]);
  fprintf(outfile, "\n\nDistribution function of job types  ");
  for (i = 1; i <= num_job_types; ++i)
    fprintf(outfile, "%8.3f", prob_distrib_job_type[i]);
  fprintf(outfile, "\n\nMean interarrival time of jobs%14.2f hours\n\n",
          mean_interarrival);
  fprintf(outfile, "Length of the simulation%20.1f eight-hour days\n\n\n",
          length_simulation);
  fprintf(outfile, "Job type     Work stations on route");
  for (i = 1; i <= num_job_types; ++i) {
    fprintf(outfile, "\n\n%4d        ", i);
    for (j = 1; j <= num_tasks[i]; ++j)
      fprintf(outfile, "%5d", route[i][j]);
  }
  fprintf(outfile, "\n\n\nJob type     ");
  fprintf(outfile, "Mean service time (in hours) for successive tasks");
  for (i = 1; i <= num_job_types; ++i) {
    fprintf(outfile, "\n\n%4d    ", i);
    for (j = 1; j <= num_tasks[i]; ++j)
      fprintf(outfile, "%9.2f", mean_service[i][j]);
  }
  

  /* Initialize all machines in all stations to the idle state. */

  for (j = 1; j <= num_stations; ++j)
    num_machines_busy[j] = 0;

  /* Initialize simlib */

  init_simlib();

  /* Set maxatr = max(maximum number of attributes per record, 4) */

  maxatr = 4; /* NEVER SET maxatr TO BE SMALLER THAN 4. */

  /* Schedule the arrival of the first job. */

  event_schedule(expon(mean_interarrival, STREAM_INTERARRIVAL), EVENT_ARRIVAL);

  /* Schedule the end of the simulation.  (This is needed for consistency of
     units.) */

  event_schedule(8 * length_simulation, EVENT_END_SIMULATION);

  /* Run the simulation until it terminates after an end-simulation event
     (type EVENT_END_SIMULATION) occurs. */

  printf("Running Simulation\n\n");

  do {
    /* Determine the next event. */

    timing();

    /* Invoke the appropriate event function. */

    switch (next_event_type) {
    case EVENT_ARRIVAL:
      arrive(1);
      break;
    case EVENT_DEPARTURE:
      depart();
      break;
    case EVENT_END_SIMULATION:
      report();
      break;
    }

    /* If the event just executed was not the end-simulation event (type
       EVENT_END_SIMULATION), continue simulating.  Otherwise, end the
       simulation. */

  } while (next_event_type != EVENT_END_SIMULATION);

  int c;

  printf("Report output in %s \nSimulation Done \n", output_file);

  fclose(infile);
  fclose(outfile);

  return 0;
}
