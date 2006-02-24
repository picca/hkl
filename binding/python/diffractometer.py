#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-

import pygtk
pygtk.require('2.0')
import gobject
import gtk
import gtk.glade

import sys

from gazpacho.loader.loader import ObjectBuilder

sys.path.append('/home/picca/Projets/hkl/build/binding/python/')

import libhkl

KEY_TAB = 65289
KEY_SUPPR = 65535

class Diffractometer:

  ############### Callbacks functions ###################

  # Window1
    def on_spinbutton_axe_value_changed(self, widget, axe):
      """ Fonction appelée lorsque l'on modifie la valeur d'un des axes """
      angle = widget.get_value()
      self.diffractometer.setAxeValue(axe, angle)
      self.update_affichage_hkl()
      return True

    def on_spinbutton_pseudoAxe_value_changed(self, widget, pseudoAxeName):
      """ Fonction appelée lorsque l'on modifie la valeur d'un des pseudoAxes """
      angle = widget.get_value()
      self.diffractometer.setpseudoAxeValue(pseudoAxeName, angle)
      self.update_affichage_axes()
      self.update_affichage_hkl()
      return True
    
    def on_entry_hkl_value_changed(self, widget):
      """ Fonction appelée lorsque l'on modifie la valeur de h, k ou de l """
      text = widget.get_text()
      try:
        v = float(text)
        widget.set_text(str(v))
      except:
        widget.set_text('')
      return False

    def on_entry_lattice_value_changed(self, spinbutton, parameter):
      """ Fonction appelé lorsque l'on modifie la valeur d'un des paramètres cristallin"""
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      index = ['a', 'b', 'c', 'alpha', 'beta', 'gamma'].index(parameter)
      lattice = list(self.diffractometer.getCrystalLattice(currentCrystalName))
      value = spinbutton.get_value()
      lattice[index] = value
      self.diffractometer.setCrystalLattice(currentCrystalName,
                                            lattice[0],
                                            lattice[1],
                                            lattice[2],
                                            lattice[3],
                                            lattice[4],
                                            lattice[5])
      self['dialog_affinement_spinbutton_' + parameter].set_value(value)

      self.update_affichage_hkl()
      self.update_affichage_reciprocal_lattice()
      self.update_affichage_UB()
      return False

    def on_entry_source_value_changed(self, widget, parameter):
      """ Fonction appelée lorsque l'on modifie la valeur d'un des paramètres de la source"""
      value = widget.get_value()
      if parameter == 'lambda':
        self.diffractometer.setWaveLength(value)
      elif parameter in ['x', 'y', 'z']:
        self.diffractometer.setIncidentBeamDirection(1, 0, 0)

      self.update_affichage_hkl()
      return False

    def on_entry_mode_parameter_value_changed(self, widget, mode, parameter):
      value = widget.get_value()
      self.diffractometer.setModeParameterValue(mode, parameter, value)
      return False

    def on_button_add_reflection_clicked(self, widget):
      """ fonction appelée lorsque l'on clique sur le bouton add reflection"""

      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      h = self['spinbutton_h'].get_value()
      k = self['spinbutton_k'].get_value()
      l = self['spinbutton_l'].get_value()
      index = self.diffractometer.addCrystalReflection(currentCrystalName,
          h, k, l, 0, True)
      row = (index,)

      for axe in self.axeNameList:
        row += (self.diffractometer.getAxeValue(axe),)

      row += (h, k, l, True,)

      self.append_row(self.reflectionmodel[currentCrystalName], None, row)
      return False

    def on_button_go_to_hkl_clicked(self, widget):
      h = self['spinbutton_h'].get_value()
      k = self['spinbutton_k'].get_value()
      l = self['spinbutton_l'].get_value()
      self.diffractometer.computeAngles(h, k, l)
      self.update_affichage_axes()
      return False

    def on_menu_quitter1_activate(self, widget):
      gtk.main_quit()
      return False

    def on_menu_nouveau_crystal_activate(self, widget):
      self['dialog_new_crystal'].show()
      return False

    def on_menu_afficher_ub1_activate(self, widget):
      self['dialog_UB'].show()
      return False

    def on_menu_preferences_activate(self, widget):
      self['dialog_preferences'].show()
      return False

    def on_menu_affiner_activate(self, widget):
      self['dialog_UB'].show()
      self['dialog_affinement'].show()
      return False

    def on_treeview_cursor_changed(self, widget):
      #print widget.get_cursor()
      return False

    def on_treeview_key_press_event(self, widget, event):
      """ 
            Cette fonction permet de supprimer les reflections en pressant la touche
            Suppr. On active ensuite les lignes qui restent pour être à même
            de les supprimer sans devoir à re-cliquer dessus
            Sans doute à revoir pour faire plus simple
            """
      if event.keyval == KEY_SUPPR:
        path = widget.get_cursor()[0]
        if path:
          currentCrystalName = self.diffractometer.getCurrentCrystalName()
          reflectionmodel = self.reflectionmodel[currentCrystalName]
          iter = reflectionmodel.get_iter(path)
          index = path[0]
          self.diffractometer.delCrystalReflection(currentCrystalName, index)
          if reflectionmodel.remove(iter):
            path = reflectionmodel.get_path(iter)
            self.update_affichage_reflections(currentCrystalName)
            widget.set_cursor(path)
          elif index > 0:
            widget.set_cursor(index-1)
      return False

    def on_CellRendererText_hkl_edited(self, widget, path, new_text, param):
      """
      Fonction appelée lorsque l'on édite les colonnes h, k ou l
      """
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      value = float(new_text)
      iter = self.reflectionmodel[currentCrystalName].get_iter(path)
      d = dict((('h', 5),('k', 6),('l', 7)))
      self.reflectionmodel[currentCrystalName].set(iter, d[param], value)

      index = int(path)
      (h, k, l, relevance, flag) = self.diffractometer.getCrystalReflectionParameters(currentCrystalName, index)
      if param == 'h':
        h = value
      elif param == 'k':
        k = value
      elif param == 'l':
        l = value
      self.diffractometer.setCrystalReflectionParameters(currentCrystalName, index, h, k, l, relevance, flag)
      return False

    def on_CellRendererToggle_toggled(self, widget, path):
      """
      Fonction appelée lorsque l'on click sur la colonne in_use
      """
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      reflectionmodel = self.reflectionmodel[currentCrystalName]
      iter = reflectionmodel.get_iter(path)
      data = reflectionmodel.get(iter, 8)
      reflectionmodel.set(iter, 8, not data[0])

      index = int(path)
      (h, k, l, relevance, flag) = self.diffractometer.getCrystalReflectionParameters(currentCrystalName, index)
      self.diffractometer.setCrystalReflectionParameters(currentCrystalName, index, h, k, l, relevance, data[0])
      return False

    def on_notebook_crystals_switch_page(self, notebook, page, page_num):
      newCurrentCrystalName = notebook.get_tab_label_text(notebook.get_nth_page(page_num))
      self.diffractometer.setCurrentCrystal(newCurrentCrystalName)
      #self['dialog_affinement_comboboxentry_crystals'].child.set_text(self.currentCrystalName)
      self.update_affichage_lattice()
      self.update_affichage_reciprocal_lattice()
      self.update_affichage_hkl()
      self.update_affichage_UB()
      return False

    def on_comboboxentry_modes_changed(self, comboboxentry):
      active = comboboxentry.get_active()
      self.currentModeName = self.modeModel[active][0]
      self.diffractometer.setCurrentMode(self.currentModeName)

  # dialog ub
    def on_dialog_ub_close_button_clicked(self, widget):
      self['dialog_UB'].hide()
      return False

  # dialog new crystal
    def on_dialog_new_crystal_add_button_clicked(self, widget):
      try:
        crystalName = self['entry_new_crystal'].get_text()
        self.diffractometer.addNewCrystal(crystalName)
        self.add_crystal(crystalName)
      except:
        print 'pas en double mec'
        return False

    def on_dialog_new_crystal_close_button_clicked(self, widget):
      self['dialog_new_crystal'].hide()
      return False

  # dialog preferences
    def on_dialog_preferences_ok_button_clicked(self, widget):
      self['dialog_preferences'].hide()
      return False

  # dialog affinement               
    def on_dialog_affinement_button_calculer_clicked(self, button):
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      crystalName = 'fit'
      try:
        self.diffractometer.copyCrystalAsNew(currentCrystalName, crystalName)
      except:
        self.diffractometer.delCrystal(currentCrystalName)
        self.diffractometer.copyCrystalAsNew(currentCrystalName, crystalName)
      a = self['dialog_affinement_spinbutton_a'].get_value()
      b = self['dialog_affinement_spinbutton_b'].get_value()
      c = self['dialog_affinement_spinbutton_c'].get_value()
      alpha = self['dialog_affinement_spinbutton_alpha'].get_value()
      beta = self['dialog_affinement_spinbutton_beta'].get_value()
      gamma = self['dialog_affinement_spinbutton_gamma'].get_value()
      self.diffractometer.setCrystalLattice(crystalName, a, b, c, alpha, beta, gamma)
      fitness = self.diffractometer.getCrystalFitness(crystalName)
      self['dialog_affinement_label_fitness'].set_text(str(fitness))
      self.diffractometer.delCrystal(crystalName)

    def on_dialog_affinement_button_affiner_clicked(self, button):
      active = self['dialog_affinement_comboboxentry_methods'].get_active()
      affinementName = self.affinementModel[active][0]
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      crystalName = 'affiner'
      try:
        self.diffractometer.copyCrystalAsNew(currentCrystalName, crystalName)
      except:
        self.diffractometer.delCrystal(crystalName)
        self.diffractometer.copyCrystalAsNew(currentCrystalName, crystalName)
      a = self['dialog_affinement_spinbutton_a'].get_value()
      b = self['dialog_affinement_spinbutton_b'].get_value()
      c = self['dialog_affinement_spinbutton_c'].get_value()
      alpha = self['dialog_affinement_spinbutton_alpha'].get_value()
      beta = self['dialog_affinement_spinbutton_beta'].get_value()
      gamma = self['dialog_affinement_spinbutton_gamma'].get_value()
      self.diffractometer.setCrystalLattice(crystalName, a, b, c, alpha, beta, gamma)
      #fittedCrystalName = self.diffractometer.affineCrystal(crystalName, affinementName)
      fitness = self.diffractometer.affineCrystal(crystalName, affinementName)
      iteration = self.diffractometer.getAffinementIteration(affinementName)
      #fitness = self.diffractometer.getCrystalfitness(fittedCrystalName)
      (a, b, c, alpha, beta, gamma) = self.diffractometer.getCrystalLattice(crystalName)
      self['dialog_affinement_label_fitness'].set_text(str(fitness))
      self['dialog_affinement_label_iteration'].set_text(str(iteration))
      self['dialog_affinement_spinbutton_a'].set_value(a)
      self['dialog_affinement_spinbutton_b'].set_value(b)
      self['dialog_affinement_spinbutton_c'].set_value(c)
      self['dialog_affinement_spinbutton_alpha'].set_value(alpha)
      self['dialog_affinement_spinbutton_beta'].set_value(beta)
      self['dialog_affinement_spinbutton_gamma'].set_value(gamma)
      self.add_crystal(crystalName)
      self.update_affichage_notebook()

    def on_dialog_affinement_comboboxentry_crystals_changed(self, comboboxentry):
      active = comboboxentry.get_active()
      currentCrystalName = self.crystalModel[active][0]
      self.diffractometer.setCurrentCrystal(currentCrystalName)
      self.update_affichage_notebook()
      return False

    ############### Non callbacks Functions ################
    def append_row(self, model, parent, columns):
      myiter=model.append(parent,None)
      for i,col in enumerate(columns):
        model.set_value(myiter, i, col)
      return myiter

    def update_affichage_all(self):
      self.update_affichage_axes()
      self.update_affichage_hkl()
      self.update_affichage_lattice()
      self.update_affichage_reciprocal_lattice()
      self.update_affichage_UB()
      self.update_affichage_source()
      self.update_affichage_fitparameters()

    def update_affichage_axes(self):
      for axeName in self.axeNameList:
        self.axeDict[axeName].set_value(self.diffractometer.getAxeValue(axeName))
        #print self.diffractometer.getAxeAngle(axe)

    def update_affichage_hkl(self):
      try:
        (h, k, l) = self.diffractometer.computeHKL()
        self['spinbutton_h'].set_value(h)
        self['spinbutton_k'].set_value(k)
        self['spinbutton_l'].set_value(l)
      except:
        for parameter in ['h', 'k', 'l']:
          self['spinbutton_'+parameter].set_value(0)

    def update_affichage_lattice(self):
      crystalName = self.diffractometer.getCurrentCrystalName()
      lattice = self.diffractometer.getCrystalLattice(crystalName)
      for i, parameter in enumerate(['a', 'b', 'c', 'alpha', 'beta', 'gamma']):
        self['spinbutton_' + parameter].set_value(lattice[i])

    def update_affichage_reciprocal_lattice(self):
      crystalName = self.diffractometer.getCurrentCrystalName()
      lattice = self.diffractometer.getCrystalReciprocalLattice(crystalName)
      parameters = ['a', 'b', 'c', 'alpha', 'beta', 'gamma']
      for i, parameter in enumerate(parameters):
        label = 'label_' + parameter + '_star'
        text = '%f' % lattice[i]
        self[label].set_text(text)

    def update_affichage_UB(self):
      crystalName = self.diffractometer.getCurrentCrystalName()
      UB = self.diffractometer.getCrystal_UB(crystalName)
      #print crystalName
      #print UB
      for i in range(3):
        for j in range(3):
          label = 'label_UB%i%i' %(i+1, j+1)
          text = '% f' % UB[i][j]
          self[label].set_text(text)

    def update_affichage_notebook(self):
      crystalList = self.diffractometer.getCrystalNames()
      currentCrystalName = self.diffractometer.getCurrentCrystalName()

      for crystal in crystalList:
        self.update_affichage_reflections(crystal)

      index = crystalList.index(currentCrystalName)
      self['notebook_crystals'].page = index

    def update_affichage_reflections(self, crystalName):
      reflectionmodel = self.reflectionmodel[crystalName]
      reflectionmodel.clear()

      nb_reflection =  self.diffractometer.getCrystalNumberOfReflection(crystalName)
      for i in range(nb_reflection):
        (h, k, l, relevance, flag) = self.diffractometer.getCrystalReflectionParameters(crystalName, i)
        row = (i,)
        for axe in self.axeNameList:
          row += (self.diffractometer.getCrystalReflectionAxeAngle(crystalName, i, axe),)
        row += (h, k, l, flag)
        self.append_row(reflectionmodel, None, row)

    def update_affichage_source(self):
      text = '%f' % self.diffractometer.getWaveLength()
      self['spinbutton_lambda'].set_value(self.diffractometer.getWaveLength())

    def update_affichage_fitparameters(self):
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      for parameterName in ['a', 'b', 'c', 'alpha', 'beta', 'gamma']:
        values = self.diffractometer.getCrystalParameterValues(currentCrystalName, parameterName)
        widgetName = 'dialog_affinement_spinbutton_' + parameterName
        self[widgetName].set_value(values[0])
        widgetName = 'dialog_affinement_spinbutton_min_' + parameterName
        self[widgetName].set_value(values[1])
        widgetName = 'dialog_affinement_spinbutton_max_' + parameterName
        self[widgetName].set_value(values[2])
        widgetName = 'dialog_affinement_checkbutton_' + parameterName
        self[widgetName].set_active(values[3])
      for parameterNames in ['euler_x', 'euler_y', 'euler_z']:
        values = self.diffractometer.getCrystalParameterValues(currentCrystalName, parameterName)
        if  not values[3]:
          flag = False
        else:
          flag = True
      self['dialog_affinement_checkbutton_U'].set_active(flag)

    def add_crystal(self, crystalName):
      self.add_crystal_tab(crystalName)
      self.crystalModel.append((crystalName,))

    def add_crystal_tab(self, crystalName):
      label = gtk.Label(crystalName)
      scrolledwindow = gtk.ScrolledWindow()
      treeview = gtk.TreeView()
      scrolledwindow.add(treeview)

      reflectionmodel =  gtk.TreeStore(gobject.TYPE_INT,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_DOUBLE,
                                       gobject.TYPE_BOOLEAN
                                       )

      self.reflectionmodel[crystalName] = reflectionmodel

      treeview.set_model(reflectionmodel)

      j = 0
      renderer=gtk.CellRendererText()
      column=gtk.TreeViewColumn('indice',renderer, text=j)
      column.set_resizable(True)
      treeview.append_column(column)

      for axeName in self.axeNameList:
        j = j + 1
        renderer=gtk.CellRendererText()
        column=gtk.TreeViewColumn(axeName,renderer, text=j)
        column.set_resizable(True)
        treeview.append_column(column)

      for parameterName in ['h', 'k', 'l']:
        j = j + 1
        renderer=gtk.CellRendererText()
        renderer.set_property('editable', True)
        renderer.set_property('editable-set', True)
        renderer.connect('edited', self.on_CellRendererText_hkl_edited, parameterName)
        column=gtk.TreeViewColumn(parameterName,renderer, text=j)
        column.set_resizable(True)
        treeview.append_column(column)

      j = j + 1
      renderer=gtk.CellRendererToggle()
      renderer.set_property('activatable', True)
      renderer.connect('toggled', self.on_CellRendererToggle_toggled)
      column=gtk.TreeViewColumn("in use",renderer,active=j)
      column.set_resizable(True)
      treeview.append_column(column)

      treeview.connect('cursor-changed', self.on_treeview_cursor_changed)
      treeview.connect('key_press_event', self.on_treeview_key_press_event)

      scrolledwindow.show_all()
      self['notebook_crystals'].append_page(scrolledwindow, label)

    def __init__(self, diffractometer):
      self.diffractometer = diffractometer
      self.axeNameList = self.diffractometer.getAxesNames()
      self.pseudoAxeNameList = self.diffractometer.getPseudoAxesNames()
      currentCrystalName = self.diffractometer.getCurrentCrystalName()
      self.currentModeName = self.diffractometer.getCurrentModeName()

      #On crée le gtk.Liststore qui va contenir la liste des noms de pseudoAxes.
      self.pseudoAxeModel = gtk.ListStore(gobject.TYPE_STRING)
      for pseudoAxeName in self.diffractometer.getPseudoAxesNames():
        self.pseudoAxeModel.append((pseudoAxeName,))

      #On crée le gtk.ListStore qui va contenir la liste des noms de modes.
      self.modeModel = gtk.ListStore(gobject.TYPE_STRING)
      for modeName in self.diffractometer.getModeNames():
        self.modeModel.append((modeName,))

      #On crée le gtk.ListStore qui va contenir la liste des noms des cristaux.
      self.crystalModel = gtk.ListStore(gobject.TYPE_STRING)
      crystalNames = self.diffractometer.getCrystalNames()
      for crystalName in crystalNames:
        self.crystalModel.append((crystalName,))

      #On crée le gtk.ListStore qui va contenir la liste des noms des methodes d'affinement.
      self.affinementModel = gtk.ListStore(gobject.TYPE_STRING)
      affinementNames = self.diffractometer.getAffinementNames()
      for affinementName in affinementNames:
        self.affinementModel.append((affinementName,))

      # On crée l'interface à partir du fichier .glade
      self.widgets = ObjectBuilder('diffractometer.gazpacho')
      #self.widgets = gtk.glade.XML('diffractometer.glade')

      ############################## Window 1

      # On cree les widget permettant de changer les valeurs des axes.
      # On cree un dictionnaire permettant d'associer un spinbutton a un nom d'axe
      self.axeDict = {}
      nb_lignes = 2
      nb_colonnes = len(self.axeNameList)
      self['table1'].resize(nb_lignes, nb_colonnes)
      for i, axeName in enumerate(self.axeNameList):
        label = gtk.Label(axeName)
        spinbutton = gtk.SpinButton(None, 1, 3)
        spinbutton.set_range(-360, 360)
        spinbutton.set_increments(1., 10.)
        spinbutton.connect('value_changed', self.on_spinbutton_axe_value_changed, axeName)
        self['table1'].attach(label, i, i+1, 0, 1)
        self['table1'].attach(spinbutton, i, i+1, 1, 2)
        self.axeDict[axeName] = spinbutton

      #On cree les widgets permettant de changer les valeurs des pseudoAxes.
      #On cree un dictionnaire permettant d'associer un spinbutton à un nom de pseudoAxe
      self.pseudoAxeDict = {}
      nb_lignes = 2
      nb_colones = len(self.pseudoAxeNameList)
      self['table7'].resize(nb_lignes, nb_colonnes)
      for i, pseudoAxeName in enumerate(self.pseudoAxeNameList):
        label = gtk.Label(pseudoAxeName)
        spinbutton.set_range(-360, 360)
        spinbutton.set_increments(1., 10.)
        spinbutton.connect('value_changed', self.on_spinbutton_pseudoAxe_value_changed, pseudoAxeName)
        self['table7'].attach(label, i, i+1, 0, 1)
        self['table7'].attach(spinbutton, i, i+1, 1, 2)
        self.pseudoAxeDict[pseudoAxeName] = spinbutton


      # dictionnaire associant un nom de crystal a un gtkStore.
      self.reflectionmodel = {}

      # On s'occupe d'ajouter les onglets au notebook
      for crystal in self.diffractometer.getCrystalNames():
        self.add_crystal_tab(crystal)

      #On met à jour les modes de calcule Dans la liste de selection
      self['comboboxentry_modes'].set_model(self.modeModel)
      self['comboboxentry_modes'].set_text_column(0)
      self['comboboxentry_modes'].child.set_text(self.currentModeName)

      # Callbacks
      for parameter in ['h', 'k', 'l']:
        w = self['spinbutton_' + parameter]
        w.connect('value_changed', self.on_entry_hkl_value_changed)

      for parameter in ['a', 'b', 'c', 'alpha', 'beta', 'gamma']:
        w = self['spinbutton_' + parameter]
        w.connect('value_changed', self.on_entry_lattice_value_changed, parameter)

      for parameter in ['lambda']:
        w = self['spinbutton_' + parameter]
        w.connect('value_changed', self.on_entry_source_value_changed, parameter)

      self['button_add_reflection'].connect('clicked', self.on_button_add_reflection_clicked)
      self['button_go_to_hkl'].connect('clicked', self.on_button_go_to_hkl_clicked)

      #self['Quitter'].connect('activate', self.on_menu_quitter1_activate)
      #self['menu_nouveau_crystal'].connect('activate', self.on_menu_nouveau_crystal_activate)
      #self['menu_afficher_ub'].connect('activate', self.on_menu_afficher_ub1_activate)
      #self['menu_preferences'].connect('activate', self.on_menu_preferences_activate)
      #self['menu_affiner'].connect('activate', self.on_menu_affiner_activate)

      self['comboboxentry_modes'].connect('changed', self.on_comboboxentry_modes_changed)

      self.notebook_crystals_handler = self['notebook_crystals'].connect('switch-page', self.on_notebook_crystals_switch_page)


      ############################## dialog new crystal
      # Callbacks 
      self['dialog_new_crystal_close_button'].connect('clicked', self.on_dialog_new_crystal_close_button_clicked)
      self['dialog_new_crystal_add_button'].connect('clicked', self.on_dialog_new_crystal_add_button_clicked)


      ############################## dialog UB
      # Callbacks
      self['dialog_ub_close_button'].connect('clicked', self.on_dialog_ub_close_button_clicked)


      ############################## dialog preference

      #On crée le notebook pour la gestion des modes
      self.notebookmode = gtk.Notebook()
      for mode in self.diffractometer.getModeNames():
        vbox = gtk.VBox()
        label = gtk.Label(self.diffractometer.getModeDescription(mode))
        vbox.add(label)
        parameters = self.diffractometer.getModeParametersNames(mode)
        nb_parameters = len(parameters)
        if nb_parameters:
          table = gtk.Table(nb_parameters, 2, False)
          for i,parameter in enumerate(parameters):
            label = gtk.Label(parameter + ': ')
            spinbutton = gtk.SpinButton(None, 1, 3)
            spinbutton.set_range(-360, 360)
            spinbutton.set_increments(1., 10.)
            spinbutton.set_value(self.diffractometer.getModeParameterValue(mode, parameter))
            spinbutton.connect('value_changed', self.on_entry_mode_parameter_value_changed, mode, parameter)
            table.attach(label, 0, 1, i, i+1)
            table.attach(spinbutton, 1, 2, i, i+1)
          vbox.pack_end(table)
        label = gtk.Label(mode)
        self.notebookmode.append_page(vbox, label)
      self['dialog_preferences_hbox'].pack_end(self.notebookmode)

      # Callbacks
      self['dialog_preferences_ok_button'].connect('clicked', self.on_dialog_preferences_ok_button_clicked)


      ############################## dialog affinement

      #On met à jour la list des crystaux dans la liste de selection
      self['dialog_affinement_comboboxentry_crystals'].set_model(self.crystalModel)
      self['dialog_affinement_comboboxentry_crystals'].set_text_column(0)
      self['dialog_affinement_comboboxentry_crystals'].child.set_text(currentCrystalName)

      #On met à jour la list des méthode d'affinement
      self['dialog_affinement_comboboxentry_methods'].set_model(self.affinementModel)
      self['dialog_affinement_comboboxentry_methods'].set_text_column(0)
      self['dialog_affinement_comboboxentry_methods'].child.set_text(affinementNames[0])


      # Callbacks
      self['dialog_affinement_comboboxentry_crystals'].connect('changed', self.on_dialog_affinement_comboboxentry_crystals_changed)
      self['dialog_affinement_button_calculer'].connect('clicked', self.on_dialog_affinement_button_calculer_clicked)
      self['dialog_affinement_button_affiner'].connect('clicked', self.on_dialog_affinement_button_affiner_clicked)



      # initialisation du diffractometer
      #On synchronize les valeurs des champs avec l'état interne de la librairie.
      self.update_affichage_all()
      self.update_affichage_notebook()

      self['window1'].show_all()
      self['dialog_preferences_hbox'].show_all()

    def __getitem__(self, key):
      return self.widgets.get_widget(key)


def main():
  gtk.main()
  return 0

if __name__ == '__main__':
  E4C = libhkl.Diffractometer_Eulerian4C()
  E4C.setWaveLength(1.654)

  E4C.addNewCrystal('crystal')
  E4C.setCrystalLattice('crystal', 18.54, 7.556, 10.04, 90., 118.6, 90.)

  E4C.setAxeValue('2theta', 30.45)    
  E4C.setAxeValue('omega', 12.4)
  E4C.setAxeValue('chi', 88.3)
  E4C.setAxeValue('phi', 0.)

  E4C.addCrystalReflection('crystal', 0., 0., 1., 0, True)

  E4C.setAxeValue('2theta', 21.)
  E4C.setAxeValue('omega', 10.95)
  E4C.setAxeValue('chi', -1.6)
  E4C.setAxeValue('phi', -2.)

  E4C.addCrystalReflection('crystal', 0. ,2. ,0., 0, True)

  E4C.setAxeValue('2theta', 53.85)    
  E4C.setAxeValue('omega', 27.339)
  E4C.setAxeValue('chi', 34.17)
  E4C.setAxeValue('phi', 56.8)

  E4C.addCrystalReflection('crystal', -2. ,2. ,1., 0, True)

  E4C.setCurrentCrystal('crystal')

  E4C.setCurrentMode('Bissector')

  diffractometer = Diffractometer(E4C)
  main()
