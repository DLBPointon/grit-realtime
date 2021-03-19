function setCar(e) {
  const obj = document.querySelector('#dategraph');
  obj.setAttribute('data', e.target.value);
}

document.querySelector('#DateGraphList').addEventListener('change', setCar);