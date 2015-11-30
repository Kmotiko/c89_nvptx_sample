#include<stdio.h>


#define WIDTH 800
#define HEIGHT 640

int calc_mandelbrot(float a, float b);

void mandelbrot(int *result, float c_x, float c_y){
  float x_pitch = 4.0f / WIDTH;
  float y_pitch = 4.0f / HEIGHT;

  float s_x = -(2.0f) + c_x;
  float s_y = 2.0f + c_y;
  float x = 0;
  float y = 0;

  // for x width
  for(int i = 0; i < WIDTH; ++i){
    x = s_x + x_pitch * i;
    // for y height
    for(int j = 0; j < HEIGHT; ++j){
      y = s_y - y_pitch * j;
      result[j * WIDTH + i] = calc_mandelbrot(x, y);
    }
  }
}

int calc_mandelbrot(float a, float b){
  float x = 0;
  float y = 0;
  float xx = 0;
  float yy = 0;

  for(int i=0; i < 1024; ++i){
    x = xx * xx - yy * yy + a;
    y = 2 * xx * yy + b;

    if( (x * x + y * y) > 4.0f ){
      return i;
    }

    xx = x;
    yy = y;
  }
  return -1;
}

void print_result(int *result){
  for(int j = 0; j < HEIGHT; j+=10){
    for(int i = 0; i < WIDTH; i+=5){
      if(result[j * WIDTH + i] == -1)
        printf("#");
      else
        printf("%d", result[j * WIDTH + i] % 9);
    }
    printf("\n");
  }
  return;
}

int main(){
  int result[HEIGHT * WIDTH];

  for(int i = 0; i < WIDTH; ++i){
    for(int j = 0; j < HEIGHT; ++j){
      result[j * WIDTH + i] = 0;
    }
  }

  mandelbrot(result, 0, 0);

  print_result(result);
  //for(int j = 0; j < HEIGHT; j+=10){
  //  for(int i = 0; i < WIDTH; i+=5){
  //    if(result[j * WIDTH + i] == -1)
  //      printf("#");
  //    else
  //      printf("%d", result[j * WIDTH + i] % 9);
  //  }
  //  printf("\n");
  //}
  
  return 0;
}
