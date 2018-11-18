import { StyleSheet } from 'react-native';

export default StyleSheet.create({
  'body': {
    'margin': [{ 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 0 }],
    'fontSize': [{ 'unit': 'rem', 'value': 1 }],
    'fontWeight': '400',
    'lineHeight': [{ 'unit': 'px', 'value': 1.5 }],
    'color': '#dfdfdf',
    'backgroundColor': '#363738',
    'textAlign': 'left'
  },
  'list > ul': {
    'color': 'rgb(26, 156, 113)',
    'listStyle': 'none'
  },
  'a': {
    'color': 'rgb(26, 156, 113)',
    'listStyle': 'none'
  },
  'a:hover': {
    'color': 'rgb(104, 104, 104)',
    'textDecoration': 'none'
  },
  'pre': {
    'backgroundColor': '#a8a9aa'
  },
  'page-link': {
    'position': 'relative',
    'display': 'block',
    'padding': [{ 'unit': 'rem', 'value': 0.5 }, { 'unit': 'rem', 'value': 0.75 }, { 'unit': 'rem', 'value': 0.5 }, { 'unit': 'rem', 'value': 0.75 }],
    'marginLeft': [{ 'unit': 'px', 'value': -1 }],
    'lineHeight': [{ 'unit': 'px', 'value': 1.25 }],
    'color': '#00c493',
    'backgroundColor': '#fff',
    'border': [{ 'unit': 'px', 'value': 1 }, { 'unit': 'string', 'value': 'solid' }, { 'unit': 'string', 'value': '#dee2e6' }]
  },
  'page-link:hover': {
    'zIndex': '2',
    'color': '#00b33c',
    'textDecoration': 'none',
    'backgroundColor': '#e9ecef',
    'borderColor': '#dee2e6'
  },
  'page-link:focus': {
    'zIndex': '2',
    'outline': '0',
    'boxShadow': [{ 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 0 }, { 'unit': 'rem', 'value': 0.2 }, { 'unit': 'string', 'value': 'rgba(0, 123, 255, 0.25)' }]
  },
  'page-link:not(:disabled):not(disabled)': {
    'cursor': 'pointer'
  },
  'page-item:first-child page-link': {
    'marginLeft': [{ 'unit': 'px', 'value': 0 }],
    'borderTopLeftRadius': '0.25rem',
    'borderBottomLeftRadius': '0.25rem'
  },
  'page-item:last-child page-link': {
    'borderTopRightRadius': '0.25rem',
    'borderBottomRightRadius': '0.25rem'
  },
  'page-itemactive page-link': {
    'zIndex': '1',
    'color': 'rgb(70, 70, 70)',
    'backgroundColor': '#00ffc8',
    'borderColor': '#00a4aa'
  },
  'page-itemdisabled page-link': {
    'color': '#6c757d',
    'pointerEvents': 'none',
    'cursor': 'auto',
    'backgroundColor': '#fff',
    'borderColor': '#dee2e6'
  },
  'pagination-lg page-link': {
    'padding': [{ 'unit': 'rem', 'value': 0.75 }, { 'unit': 'rem', 'value': 1.5 }, { 'unit': 'rem', 'value': 0.75 }, { 'unit': 'rem', 'value': 1.5 }],
    'fontSize': [{ 'unit': 'rem', 'value': 1.25 }],
    'lineHeight': [{ 'unit': 'px', 'value': 1.5 }]
  },
  'pagination-lg page-item:first-child page-link': {
    'borderTopLeftRadius': '0.3rem',
    'borderBottomLeftRadius': '0.3rem'
  },
  'pagination-lg page-item:last-child page-link': {
    'borderTopRightRadius': '0.3rem',
    'borderBottomRightRadius': '0.3rem'
  },
  'pagination-sm page-link': {
    'padding': [{ 'unit': 'rem', 'value': 0.25 }, { 'unit': 'rem', 'value': 0.5 }, { 'unit': 'rem', 'value': 0.25 }, { 'unit': 'rem', 'value': 0.5 }],
    'fontSize': [{ 'unit': 'rem', 'value': 0.875 }],
    'lineHeight': [{ 'unit': 'px', 'value': 1.5 }]
  },
  'pagination-sm page-item:first-child page-link': {
    'borderTopLeftRadius': '0.2rem',
    'borderBottomLeftRadius': '0.2rem'
  },
  'pagination-sm page-item:last-child page-link': {
    'borderTopRightRadius': '0.2rem',
    'borderBottomRightRadius': '0.2rem'
  },
  'zhadow': {
    'boxShadow': [{ 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 3 }, { 'unit': 'px', 'value': 5 }, { 'unit': 'px', 'value': -1 }, { 'unit': 'string', 'value': 'rgba(0, 0, 0, .2)' }, { 'unit': 'string', 'value': 'rgba(0, 0, 0, .2),' }, { 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 6 }, { 'unit': 'px', 'value': 10 }, { 'unit': 'px', 'value': 0 }, { 'unit': 'string', 'value': 'rgba(0, 0, 0, .14)' }, { 'unit': 'string', 'value': 'rgba(0, 0, 0, .14),' }, { 'unit': 'px', 'value': 0 }, { 'unit': 'px', 'value': 1 }, { 'unit': 'px', 'value': 18 }, { 'unit': 'px', 'value': 0 }, { 'unit': 'string', 'value': 'rgba(0, 0, 0, .12)' }]
  }
});
