import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { SociosService } from '../../services/socios';
import { CommonModule } from '@angular/common';
import { Injectable } from '@angular/core';
import jsPDF from 'jspdf';
import autoTable from 'jspdf-autotable';

@Component({
  selector: 'app-lista-prestamo-por-socio',
  imports: [CommonModule],
  templateUrl: './lista-prestamo-por-socio.html',
  styleUrl: './lista-prestamo-por-socio.css',
})
export class ListaPrestamoPorSocio {
  prestamos: any[] = [];
  nombreSocio: string = '';
  date: Date = new Date();

  constructor(private route: ActivatedRoute, private service: SociosService, private router: Router) { }

  ngOnInit(): void {
    // Capturamos el ID que enviamos desde la tabla de socios
    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.cargarPrestamos(id);
    }
  }

  generarPDFPrestamosPorSocio(idSocio: string, listaPrestamos: any[]) {
    const doc = new jsPDF('l', 'mm', 'a4');
    const pageWidth = doc.internal.pageSize.getWidth(); // Obtiene 297mm

    // --- CÁLCULO DE TOTALES (Excluyendo Ayudas) ---
    const totalCapital = listaPrestamos.reduce((acc, p) => {
      return p.tipo.toUpperCase().includes('AYUDA') ? acc : acc + Number(p.montoprestado || 0);
    }, 0);

    const totalInteres = listaPrestamos.reduce((acc, p) => {
      return p.tipo.toUpperCase().includes('AYUDA') ? acc : acc + Number(p.interestotal || 0);
    }, 0);

    // --- ENCABEZADO SIN FONDO ---
    const azulTitulo = [26, 35, 46];
    doc.setTextColor(azulTitulo[0], azulTitulo[1], azulTitulo[2]);
    doc.setFontSize(20);
    doc.setFont('helvetica', 'bold');
    doc.text('HISTORIAL COMPLETO DE PRÉSTAMOS', 15, 20);

    // --- SUBTÍTULO Y FECHA AL MARGEN ---
    doc.setFontSize(11);
    doc.setFont('helvetica', 'normal');
    const nombre = listaPrestamos[0]?.nombresocio || 'Socio';

    // Texto Izquierdo
    doc.setTextColor(100, 100, 100);
    doc.text(`SOCIO: ${nombre.toUpperCase()} (ID: ${idSocio})`, 15, 30);

    // Texto Derecho (Fecha alineada al margen de la tabla)
    const fechaTexto = `FECHA DE REPORTE: ${new Date().toLocaleDateString()}`;
    const margenDerecho = 15; // Mismo margen que el inicio (15mm)

    // Usamos align: 'right' para que el texto termine exactamente en el margen
    doc.text(fechaTexto, pageWidth - margenDerecho, 30, { align: 'right' });

    // --- GENERACIÓN DE TABLA ---
    autoTable(doc, {
      startY: 35,
      margin: { left: 15, right: 15 }, // Aseguramos márgenes simétricos
      columns: [
        { header: '#', dataKey: 'index' },
        { header: 'TIPO', dataKey: 'tipo' },
        { header: 'FECHA', dataKey: 'fechaprestamo' },
        { header: 'CAPITAL', dataKey: 'montoprestado' },
        { header: 'PLAZO', dataKey: 'plazoprestamo' },
        { header: 'INT. (%)', dataKey: 'interesprestamo' },
        { header: 'INT. TOTAL', dataKey: 'interestotal' },
        { header: 'CUOTA', dataKey: 'cuota' },
        { header: 'OBSERVACIONES', dataKey: 'comentario' },
        { header: 'ESTADO', dataKey: 'estatus_dinamico' },
      ],
      body: listaPrestamos.map((p, idx) => ({
        index: idx + 1,
        tipo: p.tipo,
        fechaprestamo: new Date(p.fechaprestamo).toLocaleDateString(),
        montoprestado: `$${Number(p.montoprestado || 0).toFixed(2)}`,
        plazoprestamo: p.plazoprestamo || 'N/A',
        interesprestamo: `${Number(p.interesprestamo || 0).toFixed(2)}%`,
        interestotal: `$${Number(p.interestotal || 0).toFixed(2)}`,
        cuota: `$${Number(p.cuota || 0).toFixed(2)}`,
        comentario: p.comentario || '',
        estatus_dinamico: p.estatus_dinamico || 'PENDIENTE'
      })),
      theme: 'grid',
      headStyles: { fillColor: [44, 62, 80], fontSize: 8 },
      styles: { fontSize: 7.5, cellPadding: 2 },
      // --- PIE DE TABLA (FOOTER) AJUSTADO ---
      foot: [[
        {
          content: 'RESUMEN TOTAL:',
          colSpan: 3,
          styles: { halign: 'right', fontStyle: 'bold', textColor: [0, 0, 0] }
        },
        {
          content: `$${totalCapital.toFixed(2)}`,
          styles: { fontStyle: 'bold', fillColor: [255, 255, 255], textColor: [0, 0, 0], fontSize: 10 }
        },
        '', '',
        {
          content: `$${totalInteres.toFixed(2)}`,
          styles: { fontStyle: 'bold', fillColor: [255, 255, 255], textColor: [0, 0, 0], fontSize: 10 }
        },
        '', '', ''
      ]],
      showFoot: 'lastPage',
      didParseCell: (data) => {
        if (data.column.dataKey === 'estatus_dinamico' && data.cell.section === 'body') {
          if (data.cell.raw === 'PAGADO') {
            data.cell.styles.textColor = [39, 174, 96];
            data.cell.styles.fontStyle = 'bold';
          } else if (data.cell.raw === 'PENDIENTE') {
            data.cell.styles.textColor = [230, 126, 34];
          }
        }
      }
    });

    doc.save(`Historial_Socio_${idSocio}.pdf`);
  }

  cargarPrestamos(id: string) {
    this.service.getPrestamosBySocio(id).subscribe({
      next: (data) => {
        this.prestamos = data;
        if (data.length > 0) {
          // Opcional: Tomar el nombre del primer registro para el título
          this.nombreSocio = data[0].nombresocio;
        }
        console.log('Prestamos cargados:', this.prestamos);
        console.log('Buscando préstamos para socio:', id);
      },
      error: (err) => console.error('Error loading loans', err)
    });
  }

  // Función para obtener la suma total de una propiedad específica
  // ... dentro de tu clase ListaPrestamoPorSocio

  getTotales(propiedad: string): number {
    return this.prestamos.reduce((acc, obj) => {
      // Convertimos a string para comparar y verificamos si NO es una ayuda
      const esAyuda = obj.tipo && String(obj.tipo).toUpperCase().startsWith('AYUDA');

      if (!esAyuda) {
        // Si no es ayuda, sumamos el valor normalmente
        return acc + (Number(obj[propiedad]) || 0);
      }

      // Si es ayuda, retornamos el acumulado sin sumar nada
      return acc;
    }, 0);
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  botonDescargarPDF() {
    // Obtenemos el id directamente de la ruta activa
    const idSocio = this.route.snapshot.paramMap.get('id');

    if (idSocio && this.prestamos.length > 0) {
      // Verificamos que los datos que tenemos pertenezcan al socio actual
      // (Asumiendo que tu objeto prestamo tiene la propiedad idsocio)
      this.generarPDFPrestamosPorSocio(idSocio, this.prestamos);
    } else {
      alert("No hay préstamos cargados para este ID.");
    }
  }

}
