

/* A special purpose main */
#include "main.h"
#include "hash.h"
#include "alloc.h"
#include "pccio.h"
#include "huffman.h"

/* Some globals that PCC needs */
int error_level, anerror;
void myexit(int n) {
  exit(n);
}
#ifdef _MSVC
#define random rand
#else
/* extern int random(void); -- Weimer: not needed! */
#endif
int __mmId;
int debugMM;
int debug;


/* Callback for writing to the compressed file */
int compressfid;
#define BUFFSIZE 1024
U8  outbuff[BUFFSIZE];
int outPoint = 0;
int written = 0;
static int flushOut(void) {
  int many;
  if(outPoint <= 0) return 0;
  many = write(compressfid, outbuff, outPoint);
  if(many != outPoint) {
    ERROR0(-1, "Error writing to the compressed file");
  }
  written += outPoint;
  outPoint = 0;
  return 0;
}
static int writeByte(U8 b) {
  outbuff[outPoint ++] = b;
  if(outPoint == BUFFSIZE) {
    flushOut();
  }
  return 0;
}
int main(int argc, char **argv) {
  PHASH freq = NewHash();
  int freqfid, codefid;
  int source, delta;
  double clk;
  int count = 0;
  int sz;
  
  /* Must be passed the name of a file to compress */
  if(argc < 2) {
    printf("Must give a file to compress\n");
    exit(1);
  }
  TIMESTART(clk);

  initLFIOFile(argv[1]);
  /* Read the file, 2 bytes at a time and create the frequency table */
  source = 0;
  while(canFetch(2)) {
    U16 wrd = fetchWordLE();
    source += 2;
    bumpFrequency(freq, wrd);
  }
  printf("Read %d bytes\n", source);
  /* Open the code and frequency files */
  freqfid = CREAT("huffman.freq");
  codefid = CREAT("huffman.code");
  compressfid = CREAT("huffman.compressed");
  if(freqfid < 0 || codefid < 0 || compressfid < 0) {
    ERROR0(1, "Cannot create frequency and code files\n");
  }
  createCompressTables(freq, freqfid, codefid);
  close(freqfid); close(codefid);
  finishIOFile();

  /* Now read again and compress */
  initCompressor("huffman.code");
  initLFIOFile(argv[1]);
  outPoint = 0; written = 0;
  startCompress(&writeByte);
  /* Read the file, 2 bytes at a time and compress */
  while(canFetch(2)) {
    U16 wrd = fetchWordLE();
    writeCompressedSymbol(wrd);
    bumpFrequency(freq, wrd);
  }
  endCompress();
  flushOut();
  close(compressfid);
  finishIOFile();

  /* Now decompress and count how many you get */
//  initLFIOFile("huffman.compressed");
//  startDecompress();
//  endDecompress();
//  delta = source;
//  while(delta > 0) {
//    fetchCompressedSymbol();
//    delta -= 2;
//  }
//  finishIOFile();

  finalizeCompressor();

  TIMESTOP(clk);
  printf("Source %d bytes. Compressed %d bytes. Ratio: %5.2lf\n",
         source, written, (double)source / (double)written);
  printf("Run hashtest in %8.3lfms\n", clk / 1000.0);
  exit (0);
}


