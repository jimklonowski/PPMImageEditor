﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.FSharp.Collections;

namespace PPMImageEditor
{
  public partial class Form1 : Form
  {
    //
    // Current image being displayed:
    //
    private PixelMap CurrentImage;

    public Form1()
    {
      InitializeComponent();
    }


    // 
    // Exit:
    private void cmdExit_Click(object sender, EventArgs e)
    {
      this.Close();
    }

    
    //
    // Open:
    //
    private void cmdOpen_Click(object sender, EventArgs e)
    {
      openFileDialog1.Filter = "PPM Files (*.ppm)|*.ppm|All files (*.*)|*.*";
      openFileDialog1.FileName = "";
      openFileDialog1.CheckFileExists = true;
      openFileDialog1.InitialDirectory = System.Environment.CurrentDirectory;

      DialogResult dr = openFileDialog1.ShowDialog();

      if (dr == System.Windows.Forms.DialogResult.OK)
      {
        string filepath = openFileDialog1.FileName;

        CurrentImage = new PixelMap(filepath);
        picImage.Image = CurrentImage.BitMap;

        // enable the other buttons so user can manipulate image:
        cmdFS1.Enabled = true;
        cmdSaveAs.Enabled = true;
        button1.Enabled = true;
        button2.Enabled = true;
        button3.Enabled = true;
        button4.Enabled = true;
        button5.Enabled = true;
        button6.Enabled = true;
      }
      else
      {
        MessageBox.Show("canceled...");
      }
    }//cmdOpen


    //
    // Test F#:
    //
    private void cmdFS1_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<int>> newImageList;

      newImageList = PPMImageLibrary.TransformFirstRowWhite(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }//cmdFS1


    //
    // Save as:
    //
    private void cmdSaveAs_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to save
        return;

      saveFileDialog1.Filter = "PPM Files (*.ppm)|*.ppm";
      saveFileDialog1.DefaultExt = "ppm";
      saveFileDialog1.FileName = "";
      saveFileDialog1.InitialDirectory = System.Environment.CurrentDirectory;

      DialogResult dr = saveFileDialog1.ShowDialog();

      if (dr == System.Windows.Forms.DialogResult.OK)
      {
        string filepath = saveFileDialog1.FileName;

        bool written = PPMImageLibrary.WriteP3Image(
          filepath,
          CurrentImage.Header.Width,
          CurrentImage.Header.Height,
          CurrentImage.Header.Depth,
          CurrentImage.ImageListData
        );

        if (!written)
          MessageBox.Show("Write failed?!");
      }
      else
      {
        MessageBox.Show("canceled...");
      }
    }//cmdSaveAs


    private void button1_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;
      
      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      

      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height,
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // Perform transformation:
      //
      FSharpList<FSharpList<int>> newImageList;

      //System.Diagnostics.Stopwatch stopWatch = new System.Diagnostics.Stopwatch();
      //stopWatch.Start();
      newImageList = PPMImageLibrary.TransformGrayscale(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );
      

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
      //stopWatch.Stop();
      //TimeSpan ts = stopWatch.Elapsed;
      //string elapsedTime = String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds);
      //MessageBox.Show("Time Elapsed: "+elapsedTime);
    }


    private void button2_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //

      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height,
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // Perform transformation:
      //
      FSharpList<FSharpList<int>> newImageList;

      newImageList = PPMImageLibrary.TransformInvert(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button3_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // Perform transformation:
      //
      FSharpList<FSharpList<int>> newImageList;

      newImageList = PPMImageLibrary.TransformFlipHorizontal(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button4_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // Perform transformation:
      //
      FSharpList<FSharpList<int>> newImageList;

      newImageList = PPMImageLibrary.TransformFlipVertical(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button5_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // Perform transformation:
      //
      FSharpList<FSharpList<int>> newImageList;

      newImageList = PPMImageLibrary.RotateRight90(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height,
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);
    }


    private void button6_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // Perform transformation:
      //
      //FSharpList<FSharpList<int>> newImageList;
      //
      //newImageList = PPMImageLibrary.TransformFirstRowWhite(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      //CurrentImage = new PixelMap(newImageList);
      //picImage.Image = CurrentImage.BitMap;
    }


  }//class
}//namespace
