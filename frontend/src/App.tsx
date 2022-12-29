import { useState, useMemo } from 'react'
import {
  createBrowserRouter,
  RouterProvider,
} from "react-router-dom";

import useMediaQuery from '@mui/material/useMediaQuery';
import { createTheme,
        ThemeProvider,
        responsiveFontSizes
      } from '@mui/material/styles';

import CssBaseline from '@mui/material/CssBaseline';
import './App.css'


import DashBoard from './routes/dashboard';

function App() {

  const prefersDarkMode = useMediaQuery('(prefers-color-scheme: dark)');

  let theme = useMemo(
    () =>
      createTheme({
        typography: {
          fontFamily: "'Plus Jakarta Sans', 'Roboto', 'Helvetica', 'Arial', 'sans-serif'"
        },
        palette: {
          primary: {
            main: '#233044',
            light: '#4d596f',
            dark: '#00061d',
            contrastText: "#fff",

          },
          secondary: {
            main: '#4782da',
            light: '#80b1ff',
            dark: '#0056a8',
            contrastText: '#fff',
          },
          mode: prefersDarkMode ? 'light' : 'light',
        },
      }),
    [prefersDarkMode],
  );

  theme = responsiveFontSizes(theme);



  const router = createBrowserRouter([
    {
      path: "/",
      element: <DashBoard />,
    },
  ]);


  return (
    <div className="App">
      <ThemeProvider theme={theme}>
        <CssBaseline />
        <RouterProvider router={router} />
      </ThemeProvider>
    </div>
  )
}

export default App


// import { useState, useMemo } from 'react'
// import {
//   createBrowserRouter,
//   RouterProvider,
// } from "react-router-dom";
//
// import useMediaQuery from '@mui/material/useMediaQuery';
// import { createTheme,
//         ThemeProvider,
//         responsiveFontSizes
//       } from '@mui/material/styles';
// import CssBaseline from '@mui/material/CssBaseline';
// import './App.css'
//
// function App() {
//   const prefersDarkMode = useMediaQuery('(prefers-color-scheme: dark)');
//
//   let theme = useMemo(
//     () =>
//       createTheme({
//         palette: {
//           mode: prefersDarkMode ? 'dark' : 'light',
//         },
//       }),
//     [prefersDarkMode],
//   );
//
//   theme = responsiveFontSizes(theme);
//
//
//   const router = createBrowserRouter([
//     {
//       path: "/",
//       element: <div>Hello world!</div>,
//     },
//   ]);
//
//
//   return (
//     <div className="App">
//       <ThemeProvider theme={theme}>
//         <CssBaseline />
//         <RouterProvider router={router} />
//       </ThemeProvider>
//     </div>
//   )
// }
//
// export default A
